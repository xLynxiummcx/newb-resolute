$input v_color0
#include <newb/config.h>
$input v_color1, v_color2, v_fogColor

#include <bgfx_shader.sh>
#define CUBEMAP 
#define ENABLE_MIE 1  
#include <newb/main.sh>
#include <resolute/includes.h>
  
#define NL_CLOUD_PARAMS(x) NL_CLOUD2##x##STEPS, NL_CLOUD2##x##THICKNESS, NL_CLOUD2##x##RAIN_THICKNESS, NL_CLOUD2##x##VELOCITY, NL_CLOUD2##x##SCALE, NL_CLOUD2##x##DENSITY, NL_CLOUD2##x##SHAPE

uniform vec4 GameSunDir;
uniform vec4 GameTimeOfDay;
uniform vec4 GameWeatherID;
uniform vec4 GameCloudHeight;
uniform vec4 GameCameraPos;
SAMPLER2D_AUTOREG(s_CloudNoise);
SAMPLER2D_AUTOREG(s_BlueNoise);
SAMPLER2D_AUTOREG(s_NoiseRGB);

#if defined(VOLUMETRIC_CLOUDS)
    
#define u_time v_color2.w     
     
#define INV_NOISERES 1.0 / 128.0;      
#define Z_STRETCH 1.0 * INV_NOISERES;      
      
float fbmn(in vec3 pos, sampler2D iChannel0) {      
    float pz = floor(pos.z);      
    float fz = pos.z - pz;      
    vec2 base = pos.xy * INV_NOISERES + pz * Z_STRETCH;      
    float a = texture2D(iChannel0, base).x;      
    float b = texture2D(iChannel0, base + vec2(Z_STRETCH)).x;      
    return mix(a, b, fz);      
}      

float cloudDensity(vec3 p, sampler2D iChannel0) {      
    vec3 pos = p;      
    
    float density = fbmn(pos * 1.2,iChannel0);      
      
    density -= fbmn(pos * 1.5,iChannel0) * 0.8;      
      
    density += fbmn(pos * 3.0,iChannel0) * 0.6;      
      
    float heightGradient = smoothstep(1.0, 3.0, p.y) * (1.0 - smoothstep(3.0, 5.5, p.y));      
  density *= heightGradient;      
      
    density = smoothstep(0.4, 0.7, density);      
      
    return clamp(density, 0.0, 1.0);      
}      
// Raymarch through clouds      
vec4 raymarchClouds(vec3 ro, vec3 rd, float maxDist,vec3 sunDir,vec3 skyColor,vec3 sunColor, sampler2D iChannel0) {      
      
    float t = 2.0;      
    vec3 col = vec3_splat(0.0);      
    float alpha = 0.0;      
      
    const int steps = 48;      
    float stepSize = 0.13;      
      
    for(int i = 0; i < steps; i++) {      
        if(alpha > 0.95) break;      
      
        vec3 pos = ro + rd * t;      
      
        if(pos.y > 0.0 && pos.y < 9.0) {      
            float density = cloudDensity(pos,iChannel0);      
      
            if(density > 0.01) {      
                float lightSample = cloudDensity(pos + sunDir * 0.5,iChannel0);      
                float lighting = mix(0.3, 1.0, 1.0 - lightSample);      
      
                float ao = mix(0.5, 6.0, 1.0 - density);      
                vec3 cloudCol = mix(skyColor,sunColor,lighting )*ao;  
  
                float scatter = pow(max(0.0, dot(rd, sunDir)), 4.0) * 0.5;      
                cloudCol += vec3(1.0, 0.9, 0.7) * scatter * density;      
      
                float a = density * 0.3;      
                col += cloudCol * a * (1.0 - alpha);      
                alpha += a * (1.0 - alpha);      
            }      
        }      
      
        t += stepSize;      
    }      
      
    return vec4(col, alpha);      
}      
      #endif
      


void main() {
    vec4 color = v_color0;
    vec3 vDir = normalize(v_color0.xyz);
    vec3 viewDir = vDir;
  float time = v_color2.w;
    
    vec3 sunDir = normalize(GameSunDir.xyz);
   
    float jitter = texture2D(s_BlueNoise, mod(gl_FragCoord.xy, 256.0) / 256.0).r;
    
    // Time of day
    vec4 whatTime = timeofday(GameTimeOfDay.x);
    float night = whatTime.x;
    float day = whatTime.w;
    float dusk = whatTime.z;
    float dawn = whatTime.y;

    // Weather
    float weather = GameWeatherID.x;
    float clear = 1.0 - smoothstep(0.0, 0.5, weather);
    float rain = smoothstep(0.5, 1.5, weather);
    float snow = smoothstep(1.5, 2.0, weather);
  sunDir = mix(sunDir, normalize(vec3(-0.5, 0.2, 0.0)), night * (1.0 - dawn) * (1.0 - dusk));      
   
    vec3 absorption;
    vec3 skyColor = mainSkyRender(vDir + GameCameraPos.xyz, sunDir, day, night, rain, dusk, dawn, snow, absorption, time);
    
    vec4 clouds = vec4_splat(0.0);
       
       float calpha;
    #ifdef FAST
    clouds = fastclouds(vDir, time, sunDir, skyColor, absorption, rain);
    skyColor = mix(skyColor,  clouds.rgb, clouds.a);
    calpha = clouds.a;
    #endif
    

 #if defined(VOLUMETRIC_CLOUDS)

    vec4 clouds = raymarchClouds(vec3(0.0,2.0,time*0.8), viewDir, 800.0,sunDir,skyColor, absorption,s_CloudNoise);    
    
    skyColor = mix(skyColor,clouds.rgb,clouds.a);     
float calpha = clouds.a;
#endif
    // Color correction
    color.rgb = colorCorrection(skyColor, rain);
    color.a = calpha; // Full opacity for sky
    
    gl_FragColor = color;
}