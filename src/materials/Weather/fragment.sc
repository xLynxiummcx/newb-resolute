$input v_fog, v_occlusionUVHeight, v_texcoord0, v_texcoord1

#include <bgfx_shader.sh>
#include <newb/main.sh>

uniform vec4 UVOffsetAndScale;
uniform vec4 OcclusionHeightOffset;
#include <resolute/includes.h>

uniform vec4 ViewPositionAndTime;
uniform vec4 FogColor;
uniform vec4 FogAndDistanceControl;
uniform vec4 GameSunDir;
uniform vec4 GameBiomeID;
uniform vec4 GameCameraPos;
uniform vec4 GameTimeOfDay;
uniform vec4 GameWeatherID;
uniform vec4 GameDimension;

SAMPLER2D_AUTOREG(s_LightingTexture);
SAMPLER2D_AUTOREG(s_OcclusionTexture);
SAMPLER2D_AUTOREG(s_WeatherTexture);

bool isOccluded(const vec2 occlUV, const float occlHeight, const float occlHeightThreshold) {
  #ifdef NO_OCCLUSION
    return false;
  #else
    #ifdef FLIP_OCCLUSION
      bool isUnder = occlHeight > occlHeightThreshold;
    #else
      bool isUnder = occlHeight < occlHeightThreshold;
    #endif
    return occlUV.x >= 0.0 && occlUV.x <= 1.0 && occlUV.y >= 0.0 && occlUV.y <= 1.0 && isUnder;
  #endif
}
void main() {
float weather = GameWeatherID.x;
vec3 whatWeather = getWeather(weather);

float rain = whatWeather.y,
      clear = whatWeather.x,
      snow = whatWeather.z;
      
  vec4 diffuse = texture2D(s_WeatherTexture, v_texcoord0);
  vec4 occlLuminanceAndHeightThreshold = texture2D(s_OcclusionTexture, v_occlusionUVHeight.xy);

  float occlLuminance = occlLuminanceAndHeightThreshold.x;
  float occlHeightThreshold = occlLuminanceAndHeightThreshold.y;
  occlHeightThreshold += occlLuminanceAndHeightThreshold.z * 255.0;
  occlHeightThreshold -= OcclusionHeightOffset.x / 255.0;

  vec2 lightingUV = vec2(0.0, 0.0);
  if (!isOccluded(v_occlusionUVHeight.xy, v_occlusionUVHeight.z, occlHeightThreshold)) {
    float mixAmount = (v_occlusionUVHeight.z - occlHeightThreshold) * 25.0;
    lightingUV = vec2(occlLuminance * (1.0 - mixAmount), 1.0);
  }

  #ifdef NL_WEATHER_SPECK
    vec2 gv = 2.0*v_texcoord1 - 1.0;
    gv = 1.0 - gv*gv;
    float g = gv.x*gv.y;
    if (UVOffsetAndScale.w > 3.5*UVOffsetAndScale.z) { // isRain
      g *= 0.9*gv.x;
    }

    vec4 color = texture2D(s_WeatherTexture, UVOffsetAndScale.xy + 0.5*UVOffsetAndScale.zw);
    color.a = g*g;

    diffuse = mix(diffuse, color, NL_WEATHER_SPECK);
  #endif 

  vec3 light = texture2D(s_LightingTexture, lightingUV).rgb;
  vec3 fogcolor = v_fog.rgb;
       fogcolor = mix(fogcolor,vec3(0.7, 0.7, 0.7),rain);
       
  diffuse.rgb *= diffuse.rgb*light;
  diffuse.rgb += 3.0*fogcolor;

float bloomThreshold = 0.6;
float bloomStrength = 0.2 + (1.2 * snow);
vec2 texelSize = vec2(1.0/64.0, 1.0/64.0);

//diffuse.rgb += bloomOptimized(s_WeatherTexture, v_texcoord0, texelSize, bloomThreshold) * bloomStrength;

// Color correction
diffuse.rgb = colorCorrection(diffuse.rgb,0.0);

diffuse.a *= lightingUV.y*(1.0-v_fog.a);
gl_FragColor = vec4(diffuse.rgb, diffuse.a * 0.65);
  }

