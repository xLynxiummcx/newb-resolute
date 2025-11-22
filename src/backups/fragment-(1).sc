#ifndef INSTANCING
$input v_texcoord0, v_posTime
#endif

#include <bgfx_shader.sh>

#ifndef INSTANCING
  #include <newb/main.sh>

  SAMPLER2D_AUTOREG(s_SkyTexture);
#endif

//bruh
/*
#if END_SKY_TYPE == 1
    float t = time;
    vec3 rd = viewDir;
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

    LightningBolt lights[2];
    getLightning(t, lights);
    lights[0].pos = cloudCenter + lights[0].pos * CLOUD_SCALE;
    lights[1].pos = cloudCenter + lights[1].pos * CLOUD_SCALE;

    vec3 LDir = vec3_splat(0.0);
    vec3 LDir2 = vec3_splat(0.0);
    vec3 terrainLightContrib = vec3_splat(0.0);

    float transmittance = 1.0;
    vec3 scatteredLight = vec3_splat(0.0);
    float totalDensity = 0.0;
    
    float terrainDist = length(v_wpos.xyz - ro);
    float distanceFog = exp(-terrainDist * 0.008); // Adjust fog density here
    
    float marchStart = 0.0;
    float marchStepSize = MARCH_SIZE * CLOUD_SCALE;

    for(int i = 0; i < MARCH_STEPS; i++) {
        if(transmittance < 0.02) break;

        float fi = float(i);
        vec3 worldPos = ro + rd * (marchStart + fi * marchStepSize);
        
        vec3 localPos = (worldPos - cloudCenter) / CLOUD_SCALE;

        if(localPos.y < -0.5 || localPos.y > 3.0) continue;

        float density = stormDensity(localPos, time);

        if(density > 0.005) {
            float heightGrad = smoothstep(-0.5, 2.5, localPos.y);
            vec3 cloudColor = mix(cloudDark, cloudMid, heightGrad);

            float thickness = pow(density, 1.2);
            cloudColor = mix(cloudColor, cloudLight, thickness * 0.3);

            LightningBolt localLights[2];
            localLights[0].pos = (lights[0].pos - cloudCenter) / CLOUD_SCALE;
            localLights[0].intensity = lights[0].intensity;
            localLights[1].pos = (lights[1].pos - cloudCenter) / CLOUD_SCALE;
            localLights[1].intensity = lights[1].intensity;
            
            vec3 lightContrib = volumetricLight(localPos, rd, localLights[0], density, time, LDir);
            lightContrib += volumetricLight(localPos, rd, localLights[1], density, time, LDir2);

            vec3 volumeColor = cloudColor + lightContrib;

            float stepTransmittance = exp(-density * CLOUD_ABSORPTION * MARCH_SIZE);
            float absorption = (1.0 - stepTransmittance);

            scatteredLight += transmittance * volumeColor * absorption;
            transmittance *= stepTransmittance;

            totalDensity += density;
        }
    }

    vec3 terrainPos = v_wpos.xyz;
    vec3 terrainNormal = N;
    vec3 toLight1 = lights[0].pos - terrainPos;
    float dist1 = length(toLight1);
    LDir = normalize(toLight1);
    float lightAtten1 = lights[0].intensity / (1.0 + dist1 * dist1 * 0.01);
    lightAtten1 *= exp(-dist1 * 0.05);    float NdotL1 = max(dot(terrainNormal, LDir), 0.0);
    
    vec3 toLight2 = lights[1].pos - terrainPos;
    float dist2 = length(toLight2);
    LDir2 = normalize(toLight2);
    float lightAtten2 = lights[1].intensity / (1.0 + dist2 * dist2 * 0.01);
    lightAtten2 *= exp(-dist2 * 0.05);
    float NdotL2 = max(dot(terrainNormal, LDir2), 0.0);
    
    vec3 lightningColor = vec3(0.7, 0.85, 1.0);
    terrainLightContrib = lightningColor * (lightAtten1 * NdotL1 + lightAtten2 * NdotL2);
    
    float cloudShadow = mix(0.3, 1.0, transmittance); // Darker under thick clouds
    terrainLightContrib *= cloudShadow;

    vec3 finalColor = skyColor * transmittance + scatteredLight;

    float totalFlash = lights[0].intensity + lights[1].intensity;
    if(totalFlash > 0.01) {
        finalColor += vec3(0.6, 0.7, 0.9) * totalFlash * 0.08 * transmittance;
    }
    
    finalColor = pow(finalColor, vec3(0.85));
    finalColor = max(finalColor - 0.02, 0.0) * 1.05;
    
    diffuse.rgb *= terrainLightContrib;
    float fogFactor = 1.0 - transmittance;
    
    float finalFog = max(fogFactor, 1.0 - distanceFog);
    diffuse.rgb = mix(diffuse.rgb, finalColor, finalFog);
    #elif END_SKY_TYPE == 2
    */
void main() {
  #ifndef INSTANCING
  vec3 color;
  float time = v_posTime.w;
  vec3 viewDir = normalize(v_posTime.xyz);
  
  vec3 flashDir; 
  color = Endflash(viewDir,flashDir,time);
    vec4 bh = renderBlackhole(viewDir, time);
    color = bh.rgb;
     color = colorCorrection(color,0.0);

    gl_FragColor = vec4(color, 1.0);
  #else
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
  #endif
}
