#ifndef INSTANCING      
  $input v_fogColor, v_worldPos, v_underwaterRainTimeDay, v_worldtestPos      
#endif      
#include <bgfx_shader.sh>      

#ifndef INSTANCING      
#define CUBEMAP      
#define ENABLE_MIE 1       
  #include <newb/main.sh>    
  #include <resolute/includes.h>
  
uniform vec4 FogColor;      
uniform vec4 FogAndDistanceControl;      
uniform vec4 GameSunDir;      
uniform vec4 GameTimeOfDay;      
uniform vec4 GameWeatherID;      
uniform vec4 GameCameraPos;  
SAMPLER2D_AUTOREG(s_SunTex);      
SAMPLER2D_AUTOREG(s_NoiseVoxel);      
SAMPLER2D_AUTOREG(s_BlueNoise);      
SAMPLER2D_AUTOREG(s_MoonTex);      
SAMPLER2D_AUTOREG(s_CloudNoise);      
SAMPLER2D_AUTOREG(s_NoiseRGB);      


#if defined(VOLUMETRIC_CLOUDS)
        
#define INV_NOISERES 1.0 / 128.0      
#define Z_STRETCH 1.0 * INV_NOISERES  
      
float fbmn(in vec3 pos, sampler2D iChannel0) {      
    float pz = floor(pos.z);      
    float fz = pos.z - pz;      
    vec2 base = pos.xy * INV_NOISERES + pz * Z_STRETCH;      
    float a = texture2D(iChannel0, base).x;      
    float b = texture2D(iChannel0, base + vec2_splat(Z_STRETCH)).x;      
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
      
      float fbm(vec2 p, sampler2D fbmtex) {
    return texture2D(fbmtex, p * 1.0/64.0).r;
}

#endif
      
void main() {      
  #ifndef INSTANCING      
    vec3 viewDir = normalize(v_worldPos);      
    nl_environment env;      
    env.end = false;      
    env.nether = false;      
    env.underwater = v_underwaterRainTimeDay.x > 0.5;      
    env.rainFactor = v_underwaterRainTimeDay.y;      
    env.dayFactor = v_underwaterRainTimeDay.w;      
    float time = v_underwaterRainTimeDay.z;      
          
    nl_skycolor skycol;      
    if (env.underwater) {      
      skycol = nlUnderwaterSkyColors(env.rainFactor, v_fogColor.rgb);      
    } else {      
      skycol = nlOverworldSkyColors(env.rainFactor, v_fogColor.rgb);      
    }      
        
    vec4 whatTime = timeofday(GameTimeOfDay.x);      
    float night = whatTime.x;      
    float day = whatTime.w;      
    float dusk = whatTime.z;      
    float dawn = whatTime.y;      
      
    float weather = GameWeatherID.x;      
    float rain  = env.rainFactor;      
    float snow  = smoothstep(1.5, 2.0, weather);      
        
    vec3 sunDir = normalize(GameSunDir.xyz);      
    sunDir = mix(sunDir, normalize(vec3(-0.5, 0.2, 0.0)), night * (1.0 - dawn) * (1.0 - dusk));      
        
    vec3 absorption;      
    vec3 skyColor = mainSkyRender(viewDir, sunDir, day, night, rain, dusk, dawn, snow, absorption, time);      
    float circle = getSunIntensity(sunDir,viewDir,night);      
          
    vec3 sun = absorption * circle;      
            
    float mask;      
    vec3 sunTex = sunTextureMovement(sunDir, viewDir, mask,s_SunTex);      
    vec3 moonTex = moonTextureMovement(normalize(vec3(-0.5, 0.2, 0.0)), viewDir, mask,s_MoonTex);      
      
    #if defined(VANILLA_SUN)      
    if(night > 0.8){      
        skyColor += moonTex;      
    } else {      
        skyColor += sunTex * absorption * 7.0;      
    }      
    #else      
    skyColor += sun;      
    #endif      
      
    float stars = renderStars(viewDir, 0.0);      
    skyColor = mix(skyColor, vec3_splat(1.0), ((1.0 - 0.3 * sin(time * 10.0)) * stars) * night);      
           
    float dither = texture2D(s_BlueNoise, mod(gl_FragCoord.xy, 256.0) / 256.0).r;      
    vec3 aurora = GetAurora(viewDir, time, dither,s_NoiseVoxel);  
      aurora = pow(aurora,vec3_splat(2.2));
  
    skyColor += ((aurora * night) * (1.0 - dawn)) * smoothstep(0.1, 1.0, viewDir.y);     
#if defined(VOLUMETRIC_CLOUDS)
vec4 clouds = raymarchClouds(vec3(0.0,2.0,time*0.05), viewDir, 800.0,sunDir,skyColor, absorption,s_CloudNoise);    
skyColor = mix(skyColor,clouds.rgb,clouds.a);     
#endif

#if !defined(VOLUMETRIC_CLOUDS)
vec2 uv1 = viewDir.xz/viewDir.y;
vec2 p = uv1 * vec2(9.0, 6.0);
p *= 0.15;
float flow = fbm(p * 0.1,s_NoiseRGB) * 1.5;
vec2 distorted = p + vec2(flow * 0.8, flow * 0.3);
float clouds2 = fbm(distorted * 0.8,s_NoiseRGB);
clouds2 += fbm(distorted * 8.0,s_NoiseRGB) * 0.3;
clouds2 /= 1.3;
clouds2 = smoothstep(max(0.4-0.3*rain,0.0), 0.7, clouds2);

#if defined(CIRRUS_CLOUD_SHADOWS)
float shadowStrength = 0.8;
float softShadows = 0.0;
float shadowSampleDist = 0.2;
const int shadowSamples = 3;

for (int i = 0; i < shadowSamples; i++) {
    float t = float(i) / float(shadowSamples);
    vec2 shadowOffset = sunDir.xz * (shadowSampleDist * t) / max(sunDir.y, 0.1);
    vec2 shadowPos = distorted + shadowOffset;
    
    float sampleCloud = fbm(shadowPos * 0.8,s_NoiseRGB);
    sampleCloud += fbm(shadowPos * 8.0) * 0.3;
    sampleCloud /= 1.3;
    sampleCloud = smoothstep(max(0.4-0.3*rain,0.0), 0.7, sampleCloud);
    
    softShadows += sampleCloud * (1.0 - t); 
}

softShadows /= float(shadowSamples);
clouds2 *= 1.0 - softShadows * shadowStrength;
#endif

clouds2 *= smoothstep(0.05,1.0,viewDir.y);
skyColor = mix(skyColor,absorption, clouds2);
#endif

#if defined(FAST_CLOUDS)
vec4  clouds = fastclouds(viewDir, time, sunDir, skyColor, absorption, rain);
skyColor = mix(skyColor,  clouds.rgb, clouds.a);
#endif

     #if defined(SHOW_LIGHTNING)   

    vec2 uv = viewDir.xy / max(viewDir.z, 0.01);      
    float flash = mod(time, 0.1);      
    float brightness = smoothstep(0.0, 0.05, flash) * smoothstep(0.3, 0.1, flash);      
    brightness += smoothstep(0.5, 0.55, flash) * smoothstep(0.7, 0.6, flash) * 0.6;      
      
    vec3 color = vec3_splat(0.0);      
    vec2 top = vec2(0.0, 0.6);      
    vec2 bottom = vec2(-0.05, -0.6);      
    float bolt1 = lightning(uv, top, bottom, 1.5);      
      
    vec2 top2 = vec2(0.05, 0.55);      
    vec2 bottom2 = vec2(0.15, -0.6);      
    float bolt2 = lightning(uv, top2, bottom2, 3.7);      
      
    float branches = 0.0;      
    branches += branch(uv, vec2(-0.01, 0.35), vec2(-0.25, 0.15), 2.1, 0.8);      
    branches += branch(uv, vec2(-0.02, 0.25), vec2(-0.15, 0.05), 3.3, 0.6);      
    branches += branch(uv, vec2(-0.03, 0.1), vec2(-0.22, -0.15), 4.5, 0.7);      
    branches += branch(uv, vec2(-0.03, -0.1), vec2(-0.18, -0.35), 5.7, 0.5);      
    branches += branch(uv, vec2(-0.04, -0.3), vec2(-0.1, -0.5), 6.2, 0.4);      
    branches += branch(uv, vec2(0.08, 0.3), vec2(0.28, 0.2), 7.1, 0.7);      
    branches += branch(uv, vec2(0.1, 0.15), vec2(0.25, -0.05), 8.3, 0.6);      
    branches += branch(uv, vec2(0.11, -0.05), vec2(0.3, -0.25), 9.5, 0.5);      
    branches += branch(uv, vec2(0.13, -0.25), vec2(0.22, -0.45), 10.2, 0.4);      
    branches += branch(uv, vec2(-0.15, 0.2), vec2(-0.3, 0.25), 11.1, 0.4);      
    branches += branch(uv, vec2(-0.12, 0.08), vec2(-0.25, -0.05), 12.3, 0.35);      
    branches += branch(uv, vec2(0.2, 0.15), vec2(0.35, 0.1), 13.5, 0.4);      
    branches += branch(uv, vec2(0.18, -0.1), vec2(0.3, -0.18), 14.7, 0.35);      
    branches += branch(uv, vec2(0.0, 0.5), vec2(-0.15, 0.65), 15.1, 0.5);      
    branches += branch(uv, vec2(0.02, 0.48), vec2(0.12, 0.68), 16.3, 0.45);      
      
    float totalLightning = bolt1 + bolt2 + branches;      
    vec3 lightningColor = mix(      
        vec3(0.6, 0.5, 1.0),      
        vec3(1.0, 0.95, 1.0),      
        pow(totalLightning, 0.8)      
    );      
      
    color = lightningColor * totalLightning * brightness;      
    float centerGlow = smoothstep(0.8, 0.0, length(uv - vec2(0.0, 0.1))) * 0.03;      
    color += vec3(0.4, 0.35, 0.6) * centerGlow * brightness;      
    skyColor += color * rain;      
    #endif      
      
    skyColor = colorCorrection(skyColor, rain);      
       
    gl_FragColor = vec4(skyColor, 1.0);      
  #else      
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);      
  #endif      
} 