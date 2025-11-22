vec3 brdf_specular(
    vec3 lightDir,      // Direction to light
    vec3 viewDir,       // Direction to camera 
    vec3 normal,        // Surface normal 
    float roughness,    // Surface roughness
    vec3 reflectance,   // Base reflectance F0
    vec3 sunCol         // Light color
) {
    vec3 H = normalize(lightDir + viewDir);

    float NdotV = max(dot(normal, viewDir), 0.0);
    float NdotL = max(dot(normal, lightDir), 0.0);
    float NdotH = max(dot(normal, H), 0.0);
    float VdotH = max(dot(viewDir, H), 0.0);

    float alpha = roughness * roughness;

    vec3  F = reflectance + (1.0 - reflectance) * pow(1.0 - VdotH, 5.0);
    float a2 = alpha * alpha;
    float denom = NdotH * NdotH * (a2 - 1.0) + 1.0;
    float D = a2 / (3.14159 * denom * denom + 1e-5);

    float k = (alpha + 1.0) * (alpha + 1.0) / 8.0;
    float Gv = NdotV / (NdotV * (1.0 - k) + k);
    float Gl = NdotL / (NdotL * (1.0 - k) + k);
    float G = Gv * Gl;

    vec3 specular = (F * D * G) / (4.0 * NdotV * NdotL + 1e-5);

    vec3 result = specular * sunCol * NdotL;

    return result;
}


vec3 brdf(vec3 lightDir, vec3 viewDir, float roughness, vec3 normal, vec3 albedo, float metallic, vec3 reflectance, vec3 sunCol) {

    float alpha = pow(roughness,2.0);
    vec3 H = normalize(lightDir + viewDir);

    float NdotV = max(dot(normal, viewDir), 0.0);
    float NdotL = max(dot(normal, lightDir), 0.23);
    float NdotH = max(dot(normal,H), 0.0);
    float VdotH = max(dot(viewDir, H), 0.0);

    vec3 F0 = reflectance;
    vec3 fresnelReflectance = F0 + (1.0 - F0) * pow(1.0 - VdotH, 5.0);

    vec3 rhoD = albedo * (vec3(1.0) - fresnelReflectance) * (1.0 - metallic);

    float k = alpha / 2.0;
    float geometry = (NdotL / (NdotL*(1.0-k)+k)) * (NdotV / (NdotV*(1.0-k)+k));

    float lowerTerm = pow(NdotH,2.0) * (pow(alpha,2.0) - 1.0) + 1.0;
    float normalDistributionFunctionGGX = pow(alpha,2.0) / (3.14159 * pow(lowerTerm,2.0) + 1e-5);

    vec3 cookTorrance = (fresnelReflectance * normalDistributionFunctionGGX * geometry) / (4.0 * NdotL * NdotV + 1e-5);

    vec3 BRDF = rhoD * NdotL + cookTorrance * sunCol;

    return min(BRDF,vec3_splat(1.0));
}

vec3 ApplyPBRLighting(
    vec4 diffuseColor,
    vec3 albedo,
    vec3 viewDir,
    vec3 viewPos,
    vec3 N,
    vec3 sunDir,
    vec3 skySunCol,
    vec3 storeSky,
    vec3 diffuseLight,
    float roughness,
    vec3 F0,
    float fresnel,
    float time,
    float dither,
    float night,
    float dawn,
    float rain,
    float jitter,
    bool isMetal,
    bool isNonMetal,
    bool doEffect,

    sampler2D s_SunTex,
    sampler2D s_NoiseVoxel
){
#if defined(ENABLE_PBR)
    if ( (isNonMetal || isMetal)) {

        vec3 viewDir_reflect = reflect(viewDir, N);
        vec3 skyReflect = storeSky;
        vec3 aurora = GetAurora(viewDir_reflect, time, dither,s_NoiseVoxel);
        skyReflect += (aurora * night) * (1.0 - dawn);

     /*   vec4 clouds = REALClouds(
            viewPos, viewDir_reflect, time, jitter,
            sunDir, skyReflect, skySunCol, 5, rain
        );
        clouds = fastclouds(viewDir_reflect, time, sunDir, skyReflect, skySunCol);
*/
        float mask;
        vec3 sunTex = sunTextureMovement(sunDir, viewDir_reflect, mask, s_SunTex);
        albedo *= 1.0 - F0;
        float strengthofRefl = mix(0.7,0.8,fresnel);
        albedo = mix(albedo,skyReflect, diffuseColor.a*strengthofRefl);
        vec3 specular = brdf_specular(sunDir, viewPos, N, roughness, F0, skySunCol);
        vec3 pbr = diffuseLight + specular;
        diffuseColor.rgb = albedo * pbr;
    }
    return diffuseColor.rgb;
   #else
   return diffuseColor.rgb;
   #endif
}

vec3 specularHighlights(vec3 viewPos,vec3 sunDir,float ndotl, vec3 sunColor,vec3 N,float strength){
const float kPi = 3.1415926;
const float specPower = 16.0;
const float kEnergyConservation = ( 8.0 + specPower) / ( 8.0 * kPi ); 
float specularStrength = strength;
vec3 halfwayDir = normalize(viewPos + sunDir);

float spec;
if(ndotl > 0.0){
spec = kEnergyConservation * pow(max(dot(N, halfwayDir), 0.0), specPower);
}
vec3 specular = sunColor * spec * 1.5;
return specular;
}

