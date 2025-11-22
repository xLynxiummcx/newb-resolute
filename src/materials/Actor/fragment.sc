$input v_color0, v_fog, v_light, v_texcoord0, v_edgemap, v_position, v_wpos

#include <bgfx_shader.sh>
#include <MinecraftRenderer.Materials/ActorUtil.dragonh>
#include <newb/main.sh>
#include <newb/functions/randoms.h>

uniform vec4 ColorBased;
uniform vec4 ChangeColor;
uniform vec4 UseAlphaRewrite;
uniform vec4 TintedAlphaTestEnabled;
uniform vec4 MatColor;
uniform vec4 OverlayColor;
uniform vec4 MultiplicativeTintColor;
uniform vec4 ActorFPEpsilon;
uniform vec4 HudOpacity;

SAMPLER2D_AUTOREG(s_MatTexture);
SAMPLER2D_AUTOREG(s_MatTexture1);

uniform vec4 ViewPositionAndTime;
uniform vec4 FogColor;
uniform vec4 FogAndDistanceControl;
uniform vec4 GameSunDir;
uniform vec4 GameBiomeID;
uniform vec4 GameCameraPos;
uniform vec4 GameTimeOfDay;
uniform vec4 GameWeatherID;
uniform vec4 GameDimension;


void main() {
  #if defined(DEPTH_ONLY) || defined(INSTANCING)
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
    return;
  #elif defined(DEPTH_ONLY_OPAQUE)
    gl_FragColor = vec4(mix(vec3_splat(1.0), v_fog.rgb, v_fog.a), 1.0);
    return;
  #endif

   float weather = GameWeatherID.x;
    nl_environment env = nlDetectEnvironment(FogColor.rgb, FogAndDistanceControl.xyz);
  
    float clear = 1.0 - smoothstep(0.0, 0.5, weather);
    float rain  = env.rainFactor;
    float snow  = smoothstep(1.5, 2.0, weather);

    vec3 whatWeather = getWeather(weather);
    vec4 whatTime = timeofday(GameTimeOfDay.x);
    float night = whatTime.x;
    float day   = whatTime.w;
    float dusk  = whatTime.z;
    float dawn  = whatTime.y;
    
            vec3 sunDir = normalize(GameSunDir.xyz);
    sunDir = mix(sunDir, normalize(vec3(-1.0, -0.2, 0.0)), night * (1.0 - dawn) * (1.0 - dusk));

    vec3 viewPos = normalize(-v_wpos);    // used across lighting
    vec3 viewDir = normalize(v_wpos);   
    
      vec3 N = normalize(cross(dFdx(v_position), dFdy(v_position)));
    
        N = mix(normalize(mul(getTBN(N), getNormal(s_MatTexture, v_texcoord0))),N,0.0);
  
  vec4 albedo = getActorAlbedoNoColorChange(v_texcoord0, s_MatTexture, s_MatTexture1, MatColor);
      float ndotl = max(dot(N, sunDir), 0.0);
      
float texalpha = albedo.a;
bool isNonMetal = false;
bool isMetal = false;

isMetal = abs(texalpha - 0.7) < 0.02;     // 0.68–0.72
isNonMetal = abs(texalpha - 0.8) < 0.02;  // 0.78–0.82

  #ifdef ALPHA_TEST
    float alpha = mix(albedo.a, (albedo.a * OverlayColor.a), TintedAlphaTestEnabled.x);
    if (shouldDiscard(albedo.rgb, alpha, ActorFPEpsilon.x)) {
      discard;
    }
  #endif

  #ifdef CHANGE_COLOR_MULTI
    albedo = applyMultiColorChange(albedo, ChangeColor.rgb, MultiplicativeTintColor.rgb);
  #elif defined(CHANGE_COLOR)
    albedo = applyColorChange(albedo, ChangeColor, albedo.a);
    albedo.a *= ChangeColor.a;
  #endif

  #ifdef ALPHA_TEST
    albedo.a = max(UseAlphaRewrite.r, albedo.a);
  #endif

  albedo.rgb *= mix(vec3(1.0, 1.0, 1.0), v_color0.rgb, ColorBased.x);

  albedo = applyOverlayColor(albedo, OverlayColor);

  albedo *= albedo;

  vec4 light = v_light;
  #if defined(EMISSIVE) || defined(EMISSIVE_ONLY)
    light.rgb = max(light.rgb, 2.0*NL_GLOW_TEX*(1.0-albedo.a)); // glow effect
  #endif

   vec3 sunColor;
    vec3 zenithsky = mainSkyRender(viewDir, sunDir, day, night, rain, dusk, dawn, snow, sunColor);
     vec3 storeSky = zenithsky;
     float sunVisibility = clamp(sunDir.y, 0.0, 1.0);
    float moonVisibility = clamp(-sunDir.y, 0.0, 1.0);

  sunColor = mix(sunColor, vec3_splat(1.0), rain);
   vec3 specular = specularHighlights(viewPos, sunDir, ndotl, sunColor, N, 0.8);
       vec3 ambient = vec3(0.2, 0.2, 0.2) + zenithsky * 0.3 + (vec3(0.65, 0.48, 1.05) * 0.8) * night * (1.0 - dawn) * (1.0 -dusk) ;

      vec3 diffuseLight =  dodiffuseLight(
    N,
    sunDir,
    viewDir,
    sunColor,
    ambient,
    0.0,
    sunVisibility,
    moonVisibility
);     
   vec3 lighting = diffuseLight;
   if(isNonMetal || isMetal){
        lighting = diffuseLight + specular;
        }
   
   if ( !env.end) {
    albedo.rgb *= lighting;
   }
   
  #ifdef TRANSPARENT
    albedo = applyHudOpacity(albedo, HudOpacity.x);
  #endif

  #ifdef NL_ENTITY_EDGE_HIGHLIGHT
    albedo.rgb *= nlEntityEdgeHighlight(v_edgemap);
  #endif

 /* albedo.rgb = mix(albedo.rgb, v_fog.rgb, v_fog.a);*/

  albedo.rgb = colorCorrection(albedo.rgb,0.0);

  gl_FragColor = albedo;
}
