$input v_color0, v_color1, v_fog, v_refl, v_texcoord0, v_lightmapUV, v_extra, v_position, v_wpos, v_ao, v_tree, v_godray, v_camdist, v_tocamPos, v_waterNormal, view, cameraDir, v_lava

#define ENABLE_MIE 0
#include <bgfx_shader.sh>
#include <newb/main.sh>
#include <newb/functions/randoms.h>

SAMPLER2D_AUTOREG(s_MatTexture);
SAMPLER2D_AUTOREG(s_SeasonsTexture);
SAMPLER2D_AUTOREG(s_LightMapTexture);
SAMPLER2D_AUTOREG(s_SunTex);
SAMPLER2D_AUTOREG(s_SSRTex);
SAMPLER2D_AUTOREG(s_Caustics);
SAMPLER2D_AUTOREG(s_Ripples);
SAMPLER2D_AUTOREG(s_NoiseVoxel);
SAMPLER2D_AUTOREG(s_Lensflare);
SAMPLER2D_AUTOREG(s_VANILLACaustics);
SAMPLER2D_AUTOREG(s_NormalsTex);
SAMPLER2D_AUTOREG(s_Puddles);

uniform vec4 ViewPositionAndTime;
uniform vec4 FogColor;
uniform vec4 FogAndDistanceControl;
uniform vec4 GameSunDir;
uniform vec4 GameBiomeID;
uniform vec4 GameCameraPos;
uniform vec4 GameTimeOfDay;
uniform vec4 GameWeatherID;
uniform vec4 GameDimension;

float fresnelSchlick(vec3 viewDir, vec3 normal, float F0) {
    float cosTheta = clamp(dot(normalize(viewDir), normalize(normal)), 0.0, 1.0);
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 2.0);
}
float pow2(float x) { return x * x; }
float pow1_5(float x) { return pow(x, 1.5); }
float clamp01(float x) { return clamp(x, 0.0, 1.0); }
float sqrt1(float x) { return sqrt(max(x, 0.0)); }

vec3 GetAurora(vec3 vDir, float time, float dither, sampler2D s_NoiseVoxel ) {
   float VdotU = clamp(vDir.y, 0.0, 1.0);
    float visibility = sqrt1(clamp01(VdotU * 4.5 - 0.225));
    visibility *= 4.0 - VdotU * 0.9;
   if (visibility <= 1.0) return vec3_splat(0.0);
 
    vec3 aurora = vec3(0.0);
    vec3 wpos = vDir;
    wpos.xz /= max(wpos.y, 0.1);
    vec2 cameraPosM = vec2(0.0);
    cameraPosM.x += time * 10.0;

    const int sampleCount = 7;
    const int sampleCountP = sampleCount + 10;

    float ditherM = dither + 10.0;
    float auroraAnimate = time * 0.0;

    for (int i = 0; i < sampleCount; i++) {
        float current = pow2((float(i) + ditherM) / float(sampleCountP));
        vec2 planePos = wpos.xz * (0.8 + current) * 10.0 + cameraPosM;
        planePos *= 0.0007;
        float noise = texture2D(s_NoiseVoxel, planePos).r;
        noise = pow2(pow2(pow2(pow2(1.0- 0.8* abs(noise - 0.5)))));
        noise *= texture2D(s_NoiseVoxel, planePos * 8.0 + auroraAnimate).b;
        noise *= texture2D(s_NoiseVoxel, planePos * 1.0 - auroraAnimate).g;
        float currentM = 1.0 - current;
        aurora += noise * currentM * mix(vec3(0.65, 0.48, 1.05), vec3(0.0, 4.5, 3.0), pow2(pow2(currentM)));
    }

    aurora *= 3.8;
    return aurora * visibility / float(sampleCount);
}
vec3 renderLensFlare(vec2 uv, vec2 lightTexcoord, vec2 dir, float offscreen)
{
    vec3 color = vec3(0.0);
    float flareCount = 6.0; // number of hex elements
    float spacing = 0.18;   // distance between elements
    float intensity = 1.2;

    for (float i = 0.0; i < flareCount; i++)
    {
        // Offset along direction
        float offset = (i / flareCount) - 0.3;
        vec2 flarePos = lightTexcoord + dir * offset;

        // Fade based on distance from center
        float falloff = 1.0 - abs(offset * 1.8);
        falloff = pow(falloff, 3.0);

        // Sample the hexagon
        vec2 sampleUV = uv - flarePos;
        sampleUV = sampleUV * 2.0 + 0.5; // scale for effect
        vec4 tex = texture2D(s_Lensflare, sampleUV);

        color += tex.rgb * falloff * intensity;
    }

    // Apply offscreen fade & distance attenuation
    color *= offscreen;
    return color;
}

// Cook–Torrance BRDF lighting
vec3 cookTorranceBRDF(vec3 N, vec3 V, vec3 L, vec3 albedo, float roughness, float metallic, vec3 lightColor) {
    vec3 H = normalize(V + L);

    // Fresnel term (Schlick's approximation)
    vec3 F0 = mix(vec3(0.04), albedo, metallic);
    float VoH = clamp(dot(V, H), 0.0, 1.0);
    vec3 F = F0 + (1.0 - F0) * pow(1.0 - VoH, 5.0);

    // Normal Distribution Function (GGX)
    float alpha = roughness * roughness;
    float NoH = clamp(dot(N, H), 0.0, 1.0);
    float alpha2 = alpha * alpha;
    float denom = NoH * NoH * (alpha2 - 1.0) + 1.0;
    float D = alpha2 / (3.14159265 * denom * denom);

    // Geometry (Smith) term
    float k = (roughness + 1.0);
    k = (k * k) / 8.0; // Schlick–GGX approximation
    float NoV = clamp(dot(N, V), 0.0, 1.0);
    float NoL = clamp(dot(N, L), 0.0, 1.0);
    float G_V = NoV / (NoV * (1.0 - k) + k);
    float G_L = NoL / (NoL * (1.0 - k) + k);
    float G = G_V * G_L;

    // Specular reflection
    vec3 spec = (D * F * G) / (4.0 * NoV * NoL + 0.001);

    // Diffuse term (Lambert, reduced by metallic)
    vec3 kD = (1.0 - F) * (1.0 - metallic);
    vec3 diffuse = kD * albedo / 3.14159265;

    // Combine
    vec3 radiance = lightColor * NoL;
    return (diffuse + spec) * radiance;
}
float hashlava(vec2 p){
    // a cheap hashlava
    p = 50.0 * fract(p * 0.3183099 + vec2(0.71,0.113));
    return fract(p.x * p.y * 95.4337);
}

float valueNoiseTile(vec2 uv, float period){
    // uv in [0..1], period in pixels will tile over period
    vec2 p = uv * period;
    vec2 i = floor(p);
    vec2 f = fract(p);
    // wrap indexes (tile)
    vec2 i00 = mod(i, period);
    vec2 i10 = mod(i + vec2(1.0, 0.0), period);
    vec2 i01 = mod(i + vec2(0.0, 1.0), period);
    vec2 i11 = mod(i + vec2(1.0, 1.0), period);
    float a = hashlava(i00);
    float b = hashlava(i10);
    float c = hashlava(i01);
    float d = hashlava(i11);
    vec2 u = f*f*(3.0-2.0*f);
    float ab = mix(a,b,u.x);
    float cd = mix(c,d,u.x);
    return mix(ab,cd,u.y);
}

float fractalTile(vec2 uv){
    float n = 0.0;
    float amp = 0.6;
    float freq = 16.0; // base period
    for(int i=0;i<5;i++){
        n += amp * valueNoiseTile(uv, freq);
        freq *= 2.0;
        amp *= 0.55;
    }
    return clamp(n, 0.0, 1.0);
}
vec3 warmGradient(float t){
    // simple warm gradient from dark brown -> orange -> yellow
    vec3 a = vec3(0.12, 0.05, 0.02);
    vec3 b = vec3(0.85, 0.54, 0.12);
    vec3 c = vec3(1.0, 0.9, 0.45);
    if(t < 0.5){
        return mix(a, b, smoothstep(0.0, 0.5, t));
    } else {
        return mix(b, c, smoothstep(0.5, 1.0, t));
    }
}
vec3 bloomOptimized(sampler2D tex, vec2 uv, vec2 texelSize, float threshold) {
    const float kernel[5] = float[](0.204164, 0.304005, 0.093913, 0.018869, 0.001999);
    
    vec3 sum = vec3_splat(0.0);

    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            int idxX = abs(x);
            int idxY = abs(y);

            vec2 offset = vec2(float(x) * texelSize.x, float(y) * texelSize.y);
            vec3 getsample = texture2D(tex, uv + offset).rgb;

            // Extract only bright areas
            getsample = max(getsample - vec3(threshold), vec3(0.0));

            // Multiply by separable Gaussian weight
            sum += getsample * kernel[idxX] * kernel[idxY];
        }
    }

    return sum;
}

void main() {
#if defined(DEPTH_ONLY_OPAQUE) || defined(DEPTH_ONLY) || defined(INSTANCING)
    gl_FragColor = vec4(1.0);
    return;
#endif
    vec3 cameraPos = GameCameraPos.xyz;
    vec2 lm = v_lightmapUV;
    float lm_y = lm.y;
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

    bool water = v_extra.b > 0.9;

    vec3 sunDir = normalize(GameSunDir.xyz);
    sunDir = mix(sunDir, normalize(vec3(-1.0, -0.2, 0.0)), night * (1.0 - dawn) * (1.0 - dusk));

    vec3 viewPos = normalize(-v_wpos);    // used across lighting
    vec3 viewDir = normalize(v_wpos);   
    float width = 0.88;
  float shadows = smoothstep(0.9, 0.8, pow(v_lightmapUV.y, 2.0));
    float sideshadow = smoothstep(0.64, 0.62, v_color1.g);
    float sunVisibility = clamp(sunDir.y, 0.0, 1.0);
    float moonVisibility = clamp(-sunDir.y, 0.0, 1.0);
    float nolight = 1.0 - lm_y;

    vec2 texuv = v_texcoord0;
    vec2 pos_xz = v_position.xz;
  
    vec4 diffuse = texture2D(s_MatTexture, texuv);
#if !defined(ALPHA_TEST)
    if (rain > 0.001 && !env.underwater) {
        // simple 1-sample ripple use (cached)
        float numFrames = 60.0;
        float frameIndex = mod(floor(ViewPositionAndTime.w * 12.0), numFrames);
        float frameHeight = 1.0 / numFrames;
        vec2 rippleUV = vec2(pos_xz.x * 0.5, pos_xz.y * 0.5 * frameHeight + frameIndex * frameHeight);
        vec3 ripple = texture2D(s_Ripples, rippleUV).rgb;
        vec2 distortion = (ripple.xy - 0.5) * 0.08;
        vec2 distortedUV = texuv + (distortion * 0.125) * (1.0 - shadows);
        vec4 rainDiffuse = texture2D(s_MatTexture, distortedUV);
        diffuse = mix(diffuse, rainDiffuse, rain);
    }
#endif
  bool nether= env.nether;
 
  float endDist = FogAndDistanceControl.z*0.6;
    bool doEffect = (v_camdist < endDist);
  
    float alpha = diffuse.a;

bool isMetal = abs(alpha - 0.7) < 0.02;     // 0.68–0.72
bool isNonMetal = abs(alpha - 0.8) < 0.02;  // 0.78–0.82

    vec4 color = v_color0;
    vec4 vertexcol = v_color1;
    float renderdistance = FogAndDistanceControl.z;
    float time = ViewPositionAndTime.w;

    // Normal calculation: compute N once; if not water, apply normal map / TBN
    vec3 N = normalize(cross(dFdx(v_position), dFdy(v_position)));
        N = normalize(mul(getTBN(N), getNormal(s_MatTexture, v_texcoord0)));
   
#ifdef ALPHA_TEST
    if (diffuse.a < 0.6) {
        discard;
    }
#endif

#if defined(SEASONS) && (defined(ALPHA_TEST) || defined(OPAQUE))
    diffuse.rgb *= mix(vec3_splat(1.0), 2.0 * texture2D(s_SeasonsTexture, v_color1.xy).rgb, vertexcol.y);
    diffuse.rgb *= vertexcol.aaa;
#else
    diffuse *= vertexcol;
#endif

    vec3 albedo = diffuse.rgb;
    
if(v_lava>0.9){
float bloomThreshold = 0.2;
float bloomStrength = 4.0;
vec2 texelSize = vec2(1.0/2048.0, 1.0/1024.0);
diffuse.rgb += bloomOptimized(s_MatTexture, texuv, texelSize, bloomThreshold) * bloomStrength;
}

      float jitter = fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453);
    vec3 rayOrigin = vec3(0.0, 6372.0e3, 0.0);
float dither = fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453);
   
    vec3 sunColor;
    vec3 zenithsky = mainSkyRender(viewDir, sunDir, day, night, rain, dusk, dawn, snow, sunColor);
     vec3 storeSky = zenithsky;
    
    
    if(nether){
    storeSky  = vec3(0.75, 0.25, 0.1);
    zenithsky = vec3(0.71, 0.23, 0.00)*1.2;
    sunColor  = vec3(1.0, 0.45, 0.1); 
    sunDir = normalize(vec3(0.3,1.0,0.3));
    }
    
    
    vec3 aurora = GetAurora(viewDir, time, dither);
      
zenithsky += (vec3(0.65, 0.48, 1.05) * 0.8) * night * (1.0 - dawn) * (1.0 -dusk) ;
    float ndotl = max(dot(N, sunDir), 0.0);

    sunColor = mix(sunColor, vec3_splat(1.0), rain);
    sunColor *=(1.0- shadows)* lm.y;
    vec3 blockLight = lightBlockCol(lm, time,v_color1.g);
    vec3 ambient = vec3(0.2, 0.2, 0.2) + zenithsky * 0.3;
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
    vec3 scattercol = vec3(0.5, 0.9, 0.4);

    // caves / nolight
    diffuse.rgb *= 1.0 - 0.8 * nolight;

    vec3 specular = specularHighlights(viewPos, sunDir, ndotl, sunColor, N, 0.8);
    float fastsss = hgPhase(dot(viewDir, sunDir), 0.76);
    vec3 lighting = diffuseLight + blockLight;
    int dimensionID = int(GameDimension.x);
    bool isTheEnd = dimensionID == 2;

    if (!isTheEnd) {
        diffuse.rgb *= lighting;
   
#if defined(SEASONS) && (defined(ALPHA_TEST) || defined(OPAQUE))
        diffuse.rgb *= fastsss * 3.0;
#endif
   diffuse.rgb *= 1.0 - 0.7 * night * lm_y;

if(nether){
if(v_lava>0.9){
   vec2 uv = pos_xz;
   float scale = 0.01;
   float base = fractalTile(uv *scale);

    float clouds = valueNoiseTile(uv * 0.05, 8.0);
    clouds = smoothstep(0.1, 0.9, clouds);
    float combined = base * (0.6+ 1.0* clouds);
      
    combined = pow(combined, 1.5);
    float blur = (fractalTile(uv * scale) - fractalTile(uv * scale + 0.002));
    float rim = smoothstep(0.01, 0.08, blur);
    vec3 col = warmGradient(combined);
   diffuse.rgb*= col * 3.0 * (combined);
 
    }
 diffuse.rgb *= 5.0;
}
    
}
    float fresnel = fresnelSchlick(viewPos, N, 0.02);

#if defined(ENABLE_WATER)
    if (water) {
    #if defined(WAVY_WATER)
    vec3 waterpos = v_position.xyz;
    
    vec2 texcoords = waterpos.xz*0.5 + 0.5;
         texcoords += waterpos.y;
         
    vec3 tex1 = texture2D(s_NormalsTex,texcoords-vec2_splat(time*0.02)).rgb;
    vec3 tex2 = texture2D(s_NormalsTex,texcoords+vec2_splat(time*0.04)).rgb ;
   N = displacement(N,v_position,time);
    #else
    N = normalize(mul(getTBN(N), getNormal(s_MatTexture, v_texcoord0)));
    #endif
   
        float ndotl_water = max(dot(N, viewPos), 0.0);
        vec3 viewDir_refl = reflect(viewDir, N);

        vec3 skySunCol;
        vec3 waterCol;
        #if !defined(WAVY_WATER)
         waterCol = diffuse.rgb;
         #else
         waterCol = mainSkyRender(viewDir_refl, sunDir, day, night, rain, dusk, dawn, snow, skySunCol);
         #endif
         
 vec3 aurora = GetAurora(viewDir_refl, time, dither);
     waterCol += (aurora * night) * (1.0 - dawn);
   
        vec4 clouds = REALClouds(rayOrigin, viewDir_refl, time, jitter, sunDir, waterCol, skySunCol, 5, rain);
        clouds = fastclouds(viewDir_refl, time, sunDir, waterCol, skySunCol);

        float mask;
        vec3 sunTex = sunTextureMovement(sunDir, viewDir_refl, mask,s_SunTex);
 
        vec3 specularW = brdf(sunDir, viewPos, 0.2, N, waterCol, 0.0, vec3_splat(0.04), skySunCol);
        diffuse.rgb = specularW;
      //   diffuse.rgb += sunTex * skySunCol*3.0;
 
        vec4 fakeenvMap = sampleBlendedFakeCubemapRoughVec4(s_SSRTex, s_SSRTex, s_SSRTex, s_SSRTex, viewDir_refl, 0.2);
        float reflalphaW = pow(dot(fakeenvMap.rgb, vec3(0.3333)) * 2.0, 1.0 + (1.0 - FogColor.b) * 4.0);
        //diffuse = mix(diffuse, vec4(fakeenvMap.rgb, reflalphaW * diffuse.a), fakeenvMap.a);
    }
#endif

    vec3 fogColor = storeSky;

    float metallic = 0.0;
    if (isMetal){
       metallic= 0.3;
       }
       
      vec3 F0 = mix(vec3(0.04), albedo, metallic);
  
float roughness;
if(isMetal){
roughness= 0.12;
}else{
roughness = 0.5;
}
   vec3 whichdirection = viewPos;
   
   
#if defined(ENABLE_PBR)
if (doEffect){
    if (isNonMetal || isMetal) {
 
        vec3 skySunCol;
        vec3 viewDir_reflect = reflect(viewDir, N);
        vec3 skyReflect = mainSkyRender(viewDir_reflect, sunDir, day, night, rain, dusk, dawn, snow, skySunCol);
    vec3 aurora = GetAurora(viewDir_reflect, time, dither);
     skyReflect+= (aurora * night) * (1.0 - dawn);
        vec4 clouds = REALClouds(rayOrigin, viewDir_reflect, time, jitter, sunDir, skyReflect, skySunCol, 5, rain);
        clouds = fastclouds(viewDir_reflect, time, sunDir, skyReflect, skySunCol);

        float mask;
        vec3 sunTex = sunTextureMovement(sunDir, viewDir_reflect, mask,s_SunTex);
      //  skyReflect += sunTex * skySunCol;
      
   albedo *= 1.0 - F0;
albedo = mix(albedo, skyReflect, diffuse.a * fresnel);
vec3 specular = brdf_specular(sunDir,viewPos,N,roughness,F0,skySunCol);
vec3 pbr = diffuseLight+specular;
diffuse.rgb = albedo;
diffuse.rgb *= pbr;
    } 
     }
#endif

    if (env.end) {
        vec3 bhDir = normalize(vec3(-4.0, -1.0, -1.5));
        vec3 endColor = vec3(1.0);
        float ndotl_end = max(dot(N, bhDir), 0.0);
        float bhdotl = max(dot(N, normalize(vec3(0.0, 1.0, 0.0))), 0.0);

        float sunVisLocal = clamp(bhDir.y, 0.0, 1.0);
        endColor *= shadows;
        endColor *= 1.0 - 0.4 * sideshadow * sunVisLocal;

        float ambient_end = 0.45;
        vec3 specular_end = specularHighlights(viewPos, bhDir, ndotl_end, endColor, N, 20.0);
        vec3 endLight = (bhdotl + ndotl_end) + ambient_end + blockLight + specular_end;

        diffuse.rgb = texture2D(s_MatTexture, v_texcoord0).rgb;
        diffuse.rgb *= texture2D(s_LightMapTexture, lm).rgb;
        diffuse.rgb += endColor * 10.0;
        diffuse.rgb *= endLight;
    }

    #if defined(REALISTIC_CAUSTICS)
   bool blockUnderWater = (v_lightmapUV.y < 0.9 && abs((2.0 * v_position.y - 15.0) / 16.0 - v_lightmapUV.y) < 0.00002);
    if (env.underwater || blockUnderWater&& !nether) {
        vec2 uv = pos_xz * 0.15;
        uv += vec2(time * 0.02, time * 0.02);
        uv += N.xz * 0.05;

        vec3 caustic = texture2D(s_Caustics, uv).rgb;
        float ndotl_c = max(N.y, 0.0);
        caustic *= ndotl_c * 0.5;

        vec3 watercol = vec3(0.0, 0.3, 0.5);
        vec3 finalColor = watercol + zenithsky*0.1 + caustic*6.0;

        diffuse.rgb *= finalColor;
        diffuse.rgb *= 2.7;
    }
#else
bool blockUnderWater = (
    v_lightmapUV.y < 0.9 &&
    abs((2.0 * v_position.y - 15.0) / 16.0 - v_lightmapUV.y) < 0.00002
);

if (env.underwater || blockUnderWater)
{
    vec2 uv = pos_xz ;

   
    uv += N.xz * 0.05;

    vec3 caustic = texture2D(s_VANILLACaustics, fract(uv)).rgb;

    float ndotl_c = max(N.y, 0.0);
    caustic *= ndotl_c * 0.5;

    vec3 watercol = vec3(0.0, 0.3, 0.5);
    vec3 finalColor = watercol + zenithsky * 0.1 + caustic * 9.0;

     diffuse.rgb *= finalColor * 2.7;
}
#endif

    if (env.underwater) {
        vec3 watercol = vec3(0.0, 0.3, 0.5);
        fogColor = watercol;
    }


#if defined(PUDDLES)
if(doEffect){
if ( rain > 0.5){
    float fresnelVal = fresnelSchlick(viewPos, N, 0.2);
    vec3 viewDir_reflect2 = reflect(viewDir, N);

    vec3 skySunCol2;
    vec3 rainsky = vec3_splat(0.7);
    vec3 skyReflect = rainsky * (0.5 + 0.5 * fresnelVal);
   float upwards = max(N.y, 0.0);
     
    float puddlemask;
    
    #if defined(SHOW_REFLECTIONS_ON_PUDDLES_ONLY)
      puddlemask = 1.0 - texture2D(s_Puddles,pos_xz*0.00925).r;
    #else
    puddlemask = 1.0;
    #endif
     puddlemask *= upwards;
    float edgeFade = smoothstep(0.0, 0.2, puddlemask);
    roughness = mix(0.3, 0.01, puddlemask);
    float gloss = 1.0 - roughness;

    vec4 fakeenvMap = sampleBlendedFakeCubemapRoughVec4(s_SSRTex, s_SSRTex, s_SSRTex, s_SSRTex, viewDir_reflect2, roughness);
    float reflalpha = pow(dot(fakeenvMap.rgb, vec3(0.3333)) * 2.0, 1.0 + (1.0 - FogColor.b) * 4.0);
    vec4 reflection = vec4_splat(0.0);
float nopuddles = 1.0;
#if !defined(ALPHA_TEST)
nopuddles= 0.0;

    reflection = mix(diffuse, vec4(fakeenvMap.rgb, reflalpha), fakeenvMap.a);
#endif
reflection.rgb = mix(reflection.rgb, skyReflect, gloss);

    vec3 puddleColor = diffuse.rgb * 0.4+ rainsky * 0.2;
    vec3 basecolor = mix(diffuse.rgb, puddleColor, puddlemask);
    vec3 aurora2 = GetAurora(viewDir_reflect2, time, dither);
     basecolor += (((aurora2 * night) * (1.0 - dawn)) * puddlemask) * fresnel;
  
    float wetness = mix(0.8, 1.0, puddlemask) * rain;

    vec3 rainTerrain = mix(
        basecolor,
        reflection.rgb * reflection.a,
        diffuse.a * fresnelVal * wetness * edgeFade
    );

    vec3 rainspecular = specularHighlights(viewPos, sunDir, ndotl, sunColor, N, 0.01 * gloss);
 

 if(!water && !env.underwater){
    diffuse.rgb = mix(diffuse.rgb, rainTerrain, rain * (1.0 - snow));
   // diffuse.rgb += (rainspecular * puddlemask) * rain;
}
 }
  }
#endif
diffuse.rgb *= 1.0 - 0.5 * rain;


    diffuse.rgb = mix(diffuse.rgb, fogColor, v_fog.a);
    diffuse.rgb = colorCorrection(diffuse.rgb, rain);
    
    gl_FragColor = diffuse;
}
