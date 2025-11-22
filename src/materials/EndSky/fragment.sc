#ifndef INSTANCING
$input v_texcoord0, v_posTime
#endif

#include <bgfx_shader.sh>

#ifndef INSTANCING
#include <newb/config.h>

#include <resolute/includes.h>

SAMPLER2D_AUTOREG(s_CloudNoise);
uniform vec4 GameCameraPos;
SAMPLER2D_AUTOREG(s_SkyTexture);
//#define PI 3.14159265359

#define INV_NOISERES  1.0 / 64.0
#define  Z_STRETCH  17.0 * INV_NOISERES

float Get3DNoise(in vec3 pos, sampler2D iChannel0 ) {
    // pack z into tile offset (fast)
    float pz = floor(pos.z);
    float fz = pos.z - pz;
    vec2 base = pos.xy * INV_NOISERES + pz * Z_STRETCH;
    // two fetches (can't avoid atlas interpolation without changing texture)
    float a = texture2D(iChannel0, base).x;
    float b = texture2D(iChannel0, base + vec2(Z_STRETCH)).x;
    return mix(a, b, fz);
}

float noise(vec3 x, sampler2D iChannel0	) {
    return Get3DNoise(x,iChannel0);
}

float stormDensity(vec3 p, float t, sampler2D iChannel0) {
    float r = length(p.xz);
    float angle = atan2(p.z, p.x);
    float height = p.y;

    if(r > 4.0 || height < -0.5 || height > 3.0) return 0.0;

    float spiral = angle - r * 0.8 + t * 0.3;

    float arms = sin(spiral * 4.0) * 0.5 + 0.5;
    arms = pow(arms, 1.5);

    float spiralBands = smoothstep(0.3, 0.7, arms);

    float eyeWall = smoothstep(0.3, 1.0, r) * smoothstep(3.5, 1.8, r);
    eyeWall = pow(eyeWall, 0.8);

    float eye = smoothstep(0.5, 0.15, r);

    float spiralTightness = 1.0 + (1.0 - smoothstep(0.0, 2.0, r)) * 2.0;
    float tightSpiral = sin((spiral + r * 0.5) * spiralTightness * 3.0) * 0.5 + 0.5;
    tightSpiral = smoothstep(0.4, 0.6, tightSpiral);

    float density = eyeWall * spiralBands * (1.0 - eye * 0.95);
    density *= (0.6 + tightSpiral * 0.4);

    float variation = noise(p * 1.5 + vec3(t * 0.05, 0.0, 0.0),iChannel0) * 0.25 + 0.75;
    density *= variation;

    float heightFalloff = exp(-abs(height - 0.4) * 1.0);
    heightFalloff *= (0.7 + 0.3 * smoothstep(2.0, 0.5, r));
    density *= heightFalloff;

    density *= smoothstep(4.0, 2.5, r);

    float tendrils = sin(spiral * 8.0 - t * 0.5) * 0.5 + 0.5;
    tendrils *= smoothstep(3.5, 2.5, r) * smoothstep(1.5, 2.5, r);
    density += tendrils * 0.3;

    return clamp(density * 0.85, 0.0, 1.0);
}


struct LightningBoltSky {
    vec3 pos;
    float intensity;
};


void getLightningSky(float t, out LightningBoltSky lights[2]) {
    float angle1 = t * 0.5;
    float flash1 = exp(-fract(t * 0.18) * 70.0);
    lights[0].pos = vec3(cos(angle1) * 2.0, 1.5, sin(angle1) * 2.0);
    lights[0].intensity = flash1 * 2.5;

    float angle2 = t * 0.6 + 2.0;
    float flash2 = exp(-fract(t * 0.13 + 0.5) * 60.0);
    lights[1].pos = vec3(cos(angle2) * 2.2, 1.8, sin(angle2) * 2.2);
    lights[1].intensity = flash2 * 2.0;
}



vec3 volumetricLight(vec3 pos, vec3 viewDir, LightningBoltSky light, float cloudDensity, sampler2D iChannel0) {
    if(light.intensity < 0.01) return vec3_splat(0.0);

    vec3 toLight = light.pos - pos;
    float lightDist = length(toLight);
    vec3 lightDir = toLight / lightDist;

    float atten = 1.0 / (1.0 + lightDist * lightDist * 0.2);
    atten *= exp(-lightDist * 0.1);

    float shadow = 1.0;
    
    float cosTheta = dot(viewDir, lightDir);
    float phase = phaseSimple(cosTheta);

    float directGlow = exp(-lightDist * 0.8) * light.intensity * 0.5;

    vec3 lightColor = vec3(0.7, 0.85, 1.0);
    vec3 scattered = lightColor * light.intensity * atten * shadow * phase * cloudDensity * 2.5;
    vec3 direct = lightColor * directGlow;

    return scattered + direct;
}

#endif

void main() {
#ifndef INSTANCING
    vec3 color;
    float time = v_posTime.w;
    
    float t = time;
      vec3 skyboxDir = v_posTime.xyz;
    

    float rotTime = 0.00174532925 * v_posTime.w;
    float sinA = sin(rotTime);
    float cosA = cos(rotTime);
    
    vec3 worldDir;
    worldDir.y = skyboxDir.y;
    worldDir.xz = vec2(
        skyboxDir.x * cosA + skyboxDir.z * sinA,
        -skyboxDir.x * sinA + skyboxDir.z * cosA
    );
    
    worldDir.xz = -worldDir.xz;
    
    vec3 rd = normalize(worldDir);

    #if END_SKY_TYPE == 1
      vec3 ro = GameCameraPos.xyz;
    
    vec3 cloudCenter = vec3(0.0, CLOUD_CENTER_HEIGHT, 0.0);
    
    vec3 skyDark = vec3(0.08, 0.10, 0.13);
    vec3 skyMid = vec3(0.15, 0.18, 0.22);
    vec3 cloudDark = vec3(0.12, 0.13, 0.15);
    vec3 cloudMid = vec3(0.25, 0.28, 0.32);
    vec3 cloudLight = vec3(0.45, 0.50, 0.55);

    float g = abs(rd.y);
    vec3 skyColor = mix(skyDark, skyMid, pow(g, 2.0));
    skyColor += vec3(0.08, 0.10, 0.12) * pow(max(rd.y, 0.0), 4.0);

    LightningBoltSky lights[2];
    getLightningSky(t, lights);
    lights[0].pos = cloudCenter + lights[0].pos * CLOUD_SCALE;
    lights[1].pos = cloudCenter + lights[1].pos * CLOUD_SCALE;

    float transmittance = 1.0;
    vec3 scatteredLight = vec3_splat(0.0);
    float totalDensity = 0.0;
    
    float marchStart = 0.0;
    float marchStepSize = MARCH_SIZE * CLOUD_SCALE;

    for(int i = 0; i < MARCH_STEPS; i++) {
        if(transmittance < 0.02) break;

        float fi = float(i);
        vec3 worldPos = ro + rd * (marchStart + fi * marchStepSize);
        
        vec3 localPos = (worldPos - cloudCenter) / CLOUD_SCALE;

        if(localPos.y < -0.5 || localPos.y > 3.0) continue;

        float density = stormDensity(localPos, t,s_CloudNoise);

        if(density > 0.005) {

            float heightGrad = smoothstep(-0.5, 2.5, localPos.y);
            vec3 cloudColor = mix(cloudDark, cloudMid, heightGrad);

            float thickness = pow(density, 1.2);
            cloudColor = mix(cloudColor, cloudLight, thickness * 0.3);

            LightningBoltSky localLights[2];
            localLights[0].pos = (lights[0].pos - cloudCenter) / CLOUD_SCALE;
            localLights[0].intensity = lights[0].intensity;
            localLights[1].pos = (lights[1].pos - cloudCenter) / CLOUD_SCALE;
            localLights[1].intensity = lights[1].intensity;
            
            vec3 lightContrib = volumetricLight(localPos, rd, localLights[0], density,s_CloudNoise);
            lightContrib += volumetricLight(localPos, rd, localLights[1], density,s_CloudNoise);

            vec3 volumeColor = cloudColor + lightContrib;

            float stepTransmittance = exp(-density * CLOUD_ABSORPTION * MARCH_SIZE);
            float absorption = (1.0 - stepTransmittance);

            scatteredLight += transmittance * volumeColor * absorption;
            transmittance *= stepTransmittance;

            totalDensity += density;
        }
    }

    vec3 finalColor = skyColor * transmittance + scatteredLight;

    float totalFlash = lights[0].intensity + lights[1].intensity;
    if(totalFlash > 0.01) {
        finalColor += vec3(0.6, 0.7, 0.9) * totalFlash * 0.08 * transmittance;
    }
    

    finalColor = pow(finalColor, vec3_splat(0.85));
    finalColor = max(finalColor - 0.02, 0.0) * 1.05;
    
    color = finalColor;
    #elif END_SKY_TYPE == 2
        vec4 bh = renderBlackhole(normalize(skyboxDir), time);
    color += bh.rgb;
    #endif
    
    gl_FragColor = vec4(color, 1.0);
#else
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
#endif
}