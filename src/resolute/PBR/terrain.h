vec3 dodiffuseLight(
    vec3 normal,
    vec3 lightDir,
    vec3 viewDir,
    vec3 lightColor,
    vec3 ambientColor,
    float shininess,
    float sunVisibility,
    float moonVisibility,
    float skyLight,
    float caves
) {
    vec3 N = normalize(normal);
    vec3 L = normalize(lightDir);
    vec3 V = normalize(viewDir);
    float NdotL = dot(N, L);
    float diff = max(NdotL, 0.15);
    float diffSoft = 0.5 * (NdotL + 1.0); // Remaps from [-1,1] to [0,1]
    vec3 directLight = sunVisibility*diff * lightColor;
    float skyLightSmooth = mix(
        pow(skyLight, 0.9),
        pow(skyLight, 0.75),  
        skyLight
    );
    
    skyLightSmooth = mix(skyLightSmooth,1.0,caves);
    float ambientOcclusion = skyLightSmooth * 0.8 + 0.75;
    vec3 dynamicAmbient = ambientColor;
    vec3 indirect = dynamicAmbient * ambientOcclusion;
    return min(indirect + directLight, vec3(1.0));
}

vec3 lightBlockCol(vec2 lm,float time, float ao,bool caves){
vec3 lightColor = vec3(0.95,0.5,0.15) * lm.x*lm.x*lm.x +0.45*lm.y;

if(caves){
return lightColor*7.0;
}else{
    return lightColor;
    }

}
vec3 warmGradient(float t){
    vec3 a = vec3(0.12, 0.05, 0.02);
    vec3 b = vec3(0.85, 0.54, 0.12);
    vec3 c = vec3(1.0, 0.9, 0.45);
    if(t < 0.5){
        return mix(a, b, smoothstep(0.0, 0.5, t));
    } else {
        return mix(b, c, smoothstep(0.5, 1.0, t));
    }
}