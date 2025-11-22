#ifndef RANDOMS_H
#define RANDOMS_H


float luminance601(vec3 color) {
    return color.r * 0.299 + color.g * 0.587 + color.b * 0.114;
}
float luminance(vec3 color) {
    return color.r * 0.299 + color.g * 0.587 + color.b * 0.114;
}

float directionallight(vec3 normal) {
    float dirfac = 0.7;
    float dir = 1.0 - dirfac * abs(normal.x);
    return dir;
}
float hash12(vec2 p) {
    const vec3 K = vec3(0.3183099, 0.3678794, 43758.5453); // 1/pi, 1/e, prime-ish
    vec3 x = fract(vec3(p.xyx) * K.x + K.y);
    x += dot(x, x.yzx + 19.19);
    return fract((x.x + x.y) * x.z);
}

float valueNoise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);

    vec2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);

    float a = hash12(i + vec2(0.0, 0.0));
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));

    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

highp float getWave(highp vec2 uv, float t){
    float gt = -t * 0.1;
    uv *= 0.75 + 0.25 * sin(t * 0.1);
    float A = sin(valueNoise(gt + uv * 0.7 - sin(uv.y * 0.4) + uv.x)) * 0.6;

    float B = cos(valueNoise(-gt + uv * 1.2 + cos(uv.y * 0.4) + uv.x)) * 0.8;

    float C = sin(valueNoise(uv * 3.0 + t * 0.6)) * 0.2;

    return (A + B );
}

float getWaterHeight(vec2 uv, float time) {
    return 0.009 * getWave(uv, time);
}

vec4 getWaterNormalMapFromHeight(vec2 uv, vec2 resolution, float scale, float time) {
    vec2 step = 1.0 / resolution;
    float height = getWaterHeight(uv, time);

    vec2 dxy = height - vec2(
        getWaterHeight(uv + vec2(step.x, 0.0), time),
        getWaterHeight(uv + vec2(0.0, step.y), time)
    );
    return vec4(normalize(vec3(dxy * scale / step, 1.0)), height);
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

    return BRDF;
}
// -------------------------------------------------------------
// Physically-Based Specular BRDF (Cook-Torrance, GGX)
// No diffuse, metallic workflow compatible
// -------------------------------------------------------------


vec3 brdf_specular(
    vec3 lightDir,      // Direction to light (normalized)
    vec3 viewDir,       // Direction to camera (normalized)
    vec3 normal,        // Surface normal (normalized)
    float roughness,    // Surface roughness [0,1]
    vec3 reflectance,   // Base reflectance F0
    vec3 sunCol         // Light color (e.g. sun)
) {
    // Half vector between light and view
    vec3 H = normalize(lightDir + viewDir);

    // Common dot products
    float NdotV = max(dot(normal, viewDir), 0.0);
    float NdotL = max(dot(normal, lightDir), 0.0);
    float NdotH = max(dot(normal, H), 0.0);
    float VdotH = max(dot(viewDir, H), 0.0);

    // Convert roughness to microfacet alpha
    float alpha = roughness * roughness;

    // Fresnel term (Schlick’s approximation)
    vec3  F = reflectance + (1.0 - reflectance) * pow(1.0 - VdotH, 5.0);

    // Normal Distribution Function (GGX)
    float a2 = alpha * alpha;
    float denom = NdotH * NdotH * (a2 - 1.0) + 1.0;
    float D = a2 / (3.14159 * denom * denom + 1e-5);

    // Geometry term (Smith-Schlick GGX)
    float k = (alpha + 1.0) * (alpha + 1.0) / 8.0;
    float Gv = NdotV / (NdotV * (1.0 - k) + k);
    float Gl = NdotL / (NdotL * (1.0 - k) + k);
    float G = Gv * Gl;

    // Cook-Torrance specular BRDF
    vec3 specular = (F * D * G) / (4.0 * NdotV * NdotL + 1e-5);

    // Apply light color & Lambert cosine term
    vec3 result = specular * sunCol * NdotL;

    return result;
}

mat3 getTBN(vec3 n,vec2 fragTexCoord,vec3 fragWorldPos) {
  /*  vec3 T = vec3(abs(normal.y) + normal.z, 0.0, normal.x);
    vec3 B = vec3(0.0, -abs(normal).x - abs(normal).z, abs(normal).y);
    vec3 N = normal;
    return transpose(mat3(T, B, N));*/
    vec2 dUVdx = dFdx(fragTexCoord);
    vec2 dUVdy = dFdy(fragTexCoord);
     vec3 dPdx = dFdx(fragWorldPos);
    vec3 dPdy = dFdy(fragWorldPos);
   
    // Calculate normal
    vec3 N = n;
    
    // Calculate tangent and bitangent
    vec3 T = normalize(dPdx * dUVdy.t - dPdy * dUVdx.t);
    vec3 B = normalize(cross(N, T));
    
    // Create TBN matrix
    mat3 TBN = mat3(T, B, N);
   return TBN;
}
float getWaveHeight(vec2 uv, float t) {
    float h = 0.0;
    float totalWeight = 0.0;

    const int N = 10;          
    float loopTime = 6.28318;  
    float timePhase = t;       

    for (int i = 0; i < N; i++) {
        // Spread wave directions, add slight random offset
        float angle = float(i) * (6.28318 / float(N)) + sin(float(i) * 1.91) * 0.1;
        vec2 dir = vec2(cos(angle), sin(angle));

        // Ocean-like frequency spectrum
        float freq = 0.2 + float(i) * 0.15; // low frequencies dominate
        float amp = 1.0 / (1.0 + float(i) * 0.4); // decaying amplitudes
        amp *= 0.7 + 0.3 * sin(float(i) * 2.33 + t * 0.1); // subtle variation over time

        // Phase ensures looping
        float phase = freq * timePhase + sin(float(i) * 3.13) * 2.0;

        h += sin(dot(uv, dir * freq) + phase) * amp;
        totalWeight += amp;
    }

    h /= totalWeight;

    // Scale down for oceanic amplitude
    return h * 0.5;
}// =======================================
// Accurate normal via finite differences
// =======================================
vec3 getWaterNormal(vec2 uv, float t) {
    float eps = 0.002;
    float scale = 7.0;
    float h  = getWaveHeight(uv, t);
    float hx = getWaveHeight(uv + vec2(eps, 0.0), t);
    float hy = getWaveHeight(uv + vec2(0.0, eps), t);

    float dx = (hx - h) / eps;
    float dy = (hy - h) / eps;

    return normalize(vec3(-dx, 1.0, -dy));
}

vec3 displacement(vec3 N, vec3 p, float time,vec2 fragTexCoord, vec3 fragWorldPos) {
    vec2 uv = p.xz;
    vec3 water = getWaterNormalMapFromHeight(uv, vec2(1024.0, 1024.0), 7.5, time).xyz;
    mat3 TBN = getTBN(N,fragTexCoord,fragWorldPos); 
    return mul(TBN,water);
}

vec3 fresnelSchlick(float cosTheta, vec3 F0) {
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}
vec3 dodiffuseLight(
    vec3 normal,
    vec3 lightDir,
    vec3 viewDir,
    vec3 lightColor,
    vec3 ambientColor,
    float shininess,
    float sunVisibility,
    float moonVisibility,
    float skyLight
) {
    vec3 N = normalize(normal);
    vec3 L = normalize(lightDir);
    vec3 V = normalize(viewDir);
    
    // Improved diffuse calculation with soft falloff
    float NdotL = dot(N, L);
    float diff = max(NdotL, 0.15);
    
    // Soft lighting for smoother transitions
    float diffSoft = 0.5 * (NdotL + 1.0); // Remaps from [-1,1] to [0,1]
    
    // Calculate direct lighting with energy conservation
    vec3 directLight = sunVisibility*diff * lightColor;
    
    // Improved indirect lighting with better sky light handling
    float skyLightSmooth = mix(
        pow(skyLight, 1.2),      // Dark areas get smoother falloff
        pow(skyLight, 0.5),      // Bright areas get linear response
        skyLight
    );
    
    // Ambient occlusion approximation
    float ambientOcclusion = skyLightSmooth * 0.8 + 1.0;
    
    // Dynamic ambient based on sun/moon visibility
    vec3 dynamicAmbient = ambientColor;/* * (
        sunVisibility * skyLightSmooth + 
        moonVisibility * pow(skyLight, 2.0) * 0.3
    );
    */
    // Final indirect lighting with physical falloff
    vec3 indirect = dynamicAmbient * ambientOcclusion;
    
    
    // Energy conservation - ensure we don't exceed physical limits
    return min(indirect + directLight, vec3(1.0));
}
vec3 fakeSSS(
    vec3 albedo,
    vec3 N,
    vec3 V,
    vec3 L,
    vec3 lightColor,
    vec3 scatterColor,
    float thickness,
    float wrap,
    float backBoost
) {
    float NdotL = max(dot(N, L), 0.0);
    float wrapped = (dot(N, L) + wrap) / (1.0 + wrap);
    wrapped = clamp(wrapped, 0.0, 1.0);
    float diffuse = mix(NdotL, wrapped, thickness);

    float NdotLneg = max(-dot(N, L), 0.0);
    float backScatter = pow(NdotLneg, 1.5) * backBoost * thickness;

    float NdotV = max(dot(N, V), 0.0);
    float viewBoost = pow(1.0 - NdotV, 2.0) * thickness;

    vec3 scatter = scatterColor * (diffuse * 0.5 + backScatter * 0.8 + viewBoost * 0.5) * lightColor;
    float scatterMix = smoothstep(0.0, 1.0, thickness);

    vec3 outColor = mix(albedo * diffuse * lightColor, albedo * diffuse * lightColor + scatter, scatterMix);
    return clamp(outColor, 0.0, 1.0);
}

float Bayer2(vec2 a) {
    a = floor(a);
    return fract(a.x / 2. + a.y * a.y * .75);
}

// doksli dither: https://www.shadertoy.com/view/7sfXDn
#define Bayer4(a)   (Bayer2 (.5 *(a)) * .25 + Bayer2(a))
#define Bayer8(a)   (Bayer4 (.5 *(a)) * .25 + Bayer2(a))
#define Bayer16(a)  (Bayer8 (.5 *(a)) * .25 + Bayer2(a))
#define Bayer32(a)  (Bayer16(.5 *(a)) * .25 + Bayer2(a))
#define Bayer64(a)  (Bayer32(.5 *(a)) * .25 + Bayer2(a))
vec3 sunPosition() {
     float rot = 90.0;
    float sunAngle = rot;
     float sunX = 0.1;
    float sunY = sin(radians(sunAngle));
    float sunZ = cos(radians(sunAngle));
    return vec3(sunX, sunY, sunZ);
}

vec3 getWeather(float weather){
float clear = 1.0 - smoothstep(0.0, 0.5, weather);
float rain  = smoothstep(0.5, 1.5, weather);
float snow  = smoothstep(1.5, 2.0, weather);

return vec3(clear,rain,snow);
}
#define RESOLUTION 256.0

vec3 getNormal(sampler2D TEXTURE_0, vec2 coord) {
    float offsets = 1.0 / float(RESOLUTION) / 64.0;
    
    // 3x3 Sobel sampling
    float tl = luminance601(texture2D(TEXTURE_0, coord + vec2(-offsets, -offsets)).rgb);
    float t  = luminance601(texture2D(TEXTURE_0, coord + vec2(0.0, -offsets)).rgb);
    float tr = luminance601(texture2D(TEXTURE_0, coord + vec2(offsets, -offsets)).rgb);
    float l  = luminance601(texture2D(TEXTURE_0, coord + vec2(-offsets, 0.0)).rgb);
    float r  = luminance601(texture2D(TEXTURE_0, coord + vec2(offsets, 0.0)).rgb);
    float bl = luminance601(texture2D(TEXTURE_0, coord + vec2(-offsets, offsets)).rgb);
    float b  = luminance601(texture2D(TEXTURE_0, coord + vec2(0.0, offsets)).rgb);
    float br = luminance601(texture2D(TEXTURE_0, coord + vec2(offsets, offsets)).rgb);
    
    // Sobel filter
    float dx = (tr + 2.0*r + br) - (tl + 2.0*l + bl);
    float dy = (bl + 2.0*b + br) - (tl + 2.0*t + tr);
    
    vec2 gradient = vec2(dx, dy) * 0.75;
    
    float lenSq = dot(gradient, gradient);
    vec3 normal = vec3(gradient, sqrt(max(0.0, 1.0 - lenSq)));
    
    return normalize(normal);
}

float detectTexture(inout bool reflective, inout bool slightly,float alpha){
#if !defined(TRANSPARENT) && !defined(ALPHA_TEST)
//opacity - 0.97
bool detecttexture2D = alpha > 0.965 && alpha < 0.975;
//opacity - 0.99
bool detect = alpha > 0.985 && alpha < 0.995;

if(detecttexture2D){
reflective = true;
}
if(detect){
slightly = true;
}
#endif

return 0.0;
}
vec3 lightBlockCol(vec2 lm,float time, float ao){
//vec3 lightColor = vec3(1.0, 0.72, 0.32) * lm.x*lm.x*lm.x;


// Detect light source type by intensity
float blockLight = lm.x; // This is your lm.x value (0.0 - 1.0)

// Define light level thresholds (with some tolerance)
const float REDSTONE_LIGHT = 7.0 / 15.0;  // ≈ 0.467
const float SOUL_LIGHT = 10.0 / 15.0;     // ≈ 0.667
const float TORCH_LIGHT = 14.0 / 15.0;    // ≈ 0.933
const float TOLERANCE = 0.05;              // Margin for matching

vec3 colorTint = vec3(1.0); // Default: no tint

// Check for redstone torch (light level 7)
if (abs(blockLight - REDSTONE_LIGHT) < TOLERANCE && blockLight > 0.1) {
    colorTint = vec3(1.0, 0.4, 0.3); // Reddish tint
}
// Check for soul torch (light level 10)
else if (abs(blockLight - SOUL_LIGHT) < TOLERANCE) {
    colorTint = vec3(0.3, 0.7, 1.0); // Cyan/blue tint
}
// Regular torch or bright lights (level 14-15)
else if (blockLight > 0.85) {
    colorTint = vec3(1.0, 0.9, 0.7); // Warm orange
}

// Apply the tint
vec3 lightColor = colorTint;

return lightColor * lm.x;
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

// ------------------------
// Helper: sample cubemap approximation (unchanged semantics)
// ------------------------
vec4 sampleBlendedFakeCubemapRoughVec4(
    sampler2D texPosX,
    sampler2D texNegX,
    sampler2D texPosZ,
    sampler2D texNegZ,
    vec3 dir,
    float roughness
) {
    dir = normalize(dir);
    vec3 absDir = abs(dir);

    // Precompute denom once
    float denomXZ = absDir.x + absDir.z + 1e-5;
    float xWeight = absDir.x / denomXZ;
    float zWeight = absDir.z / denomXZ;

    float lod = roughness * 5.0;

    // compute uvs safely (if absDir.x or z near zero, uv values still defined due to +1e-5 above)
    vec2 uvPX = dir.zy / absDir.x * 0.5 + 0.5;
    vec2 uvNX = -dir.zy / absDir.x * 0.5 + 0.5;
    vec2 uvPZ = vec2(dir.x, -dir.y) / absDir.z * 0.5 + 0.5;
    vec2 uvNZ = vec2(-dir.x, -dir.y) / absDir.z * 0.5 + 0.5;

    vec4 cPX = textureLod(texPosX, uvPX, lod);
    vec4 cNX = textureLod(texNegX, uvNX, lod);
    vec4 cPZ = textureLod(texPosZ, uvPZ, lod);
    vec4 cNZ = textureLod(texNegZ, uvNZ, lod);

    vec4 xBlend = mix(cPZ, cPZ, dir.x * 0.5 + 0.5);
    vec4 zBlend = mix(cPZ, cPZ, dir.z * 0.5 + 0.5);

    return xWeight * xBlend +zWeight * zBlend;
    
   /* vec3 horDir = normalize(vec3(dir.x, 0.0, dir.z));

    // Four cardinal directions in horizontal plane
    vec3 rightDir   = vec3(1.0, 0.0, 0.0);
    vec3 backDir    = vec3(0.0, 0.0, -1.0);
    vec3 forwardDir = vec3(0.0, 0.0, 1.0);
    vec3 leftDir    = vec3(-1.0, 0.0, 0.0);

    // Compute blending weights (clamped to [0,1])
    float wRight   = max(dot(horDir, rightDir), 0.0);
    float wBack    = max(dot(horDir, backDir), 0.0);
    float wForward = max(dot(horDir, forwardDir), 0.0);
    float wLeft    = max(dot(horDir, leftDir), 0.0);

    // Normalize weights
    float total = wRight + wBack + wForward + wLeft;
    wRight   /= total;
    wBack    /= total;
    wForward /= total;
    wLeft    /= total;

    // Map each side to its UV region in the 2x2 texture layout
    vec2 uvRight   = vec2(0.25, 0.75);  // Right is top-left quarter
    vec2 uvBack    = vec2(0.75, 0.75);  // Back is top-right quarter
    vec2 uvForward = vec2(0.25, 0.25);  // Forward is bottom-left
    vec2 uvLeft    = vec2(0.75, 0.25);  // Left is bottom-right

    // Sample the texture at the center of each quadrant
    vec4 colRight   = texture2D(texPosX, uvRight);
    vec4 colBack    = texture2D(texPosX, uvBack);
    vec4 colForward = texture2D(texPosX, uvForward);
    vec4 colLeft    = texture2D(texPosX, uvLeft);

    // Blend the four samples by weight
    vec4 color = colRight * wRight +
                 colBack * wBack +
                 colForward * wForward +
                 colLeft * wLeft;

    return color;*/
}

vec3 sunTextureMovement(
    vec3 sunDir, // sun direction
    vec3 rayDir, // view direction
    out float mask, sampler2D s_SunTex // output mask
) {
    vec3 forward = sunDir;
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);

    float vdotl = dot(rayDir, sunDir);
    vec2 uv = vec2(dot(rayDir, right), dot(rayDir, up));

    const float sunSize = NL_SUN_SIZE;
    uv = uv / sunSize * 0.5 + 0.5;

    mask = 0.0;
    vec3 sunColor = vec3(0.0);

    sunColor = texture2D(s_SunTex, uv).rgb;
    float lum = dot(sunColor, vec3(0.299, 0.587, 0.114));
    float maskLum = smoothstep(0.0, 0.9, lum);
    float distMask = smoothstep(0.5, 0.48, length(uv - 0.5));

    mask = maskLum * distMask;
    if (vdotl <= 0.0) {
        sunColor = vec3(0.0);
    }
    return sunColor * mask;
}

vec3 moonTextureMovement(
	vec3 sunDir,//sun direction
	vec3 rayDir,//view direction
	out float mask, sampler2D s_MoonTex//outut mask for blending
	){
    vec3 forward = sunDir;
    vec3 right = normalize(cross(vec3(0.0,1.0,0.0), forward));
    vec3 up = cross(forward, right);
    float vdotl = dot(rayDir,sunDir);
    // Project rayDir into sun plane
    vec2 uv = vec2(dot(rayDir, right), dot(rayDir, up));
    float sunSize = 0.4;
    uv = uv / sunSize * 0.5 + 0.5;

    mask = 0.0;
    vec3 sunColor = vec3(0.0);
    float alpha = 0.0;


      sunColor = texture2D(s_MoonTex, uv).rgb;
      float dist = length(uv - 0.5);

//remove blackness from texture (shader editor;
// don't know if the same issue will occur in Minecraft)
float lum = dot(sunColor, vec3(0.299,0.587,0.114));
float maskLum = smoothstep(0.0, 0.9, lum);
float distMask = smoothstep(0.5, 0.48, length(uv - 0.5));
mask = maskLum * distMask;
if(vdotl <=0.0){
sunColor = vec3(0.0);
}
return sunColor * mask;
}

float fresnelSchlick(vec3 viewDir, vec3 normal, float F0) {
    float cosTheta = clamp(dot(normalize(viewDir), normalize(normal)), 0.0, 1.0);
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 3.0);
}
float pow2(float x) { return x * x; }
float pow1_5(float x) { return pow(x, 1.5); }
float clamp01(float x) { return clamp(x, 0.0, 1.0); }
float sqrt1(float x) { return sqrt(max(x, 0.0)); }

vec3 GetAurora(vec3 vDir, float time, float dither, sampler2D s_NoiseVoxel) {
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

vec4 textureCube2x3(sampler2D atlas, vec3 dir) {
    vec3 n = normalize(dir);
    float ax = abs(n.x);
    float ay = abs(n.y);
    float az = abs(n.z);

    float m = 0.0;
    vec2 uv = vec2(0.0);
    int face = 0;

    if (ax >= ay && ax >= az) {
        m = ax;
        if (n.x > 0.0) { face = 1; uv = vec2( n.z, n.y); }  // +X Right
        else           { face = 4; uv = vec2( -n.z, n.y); }  // -X Left
    }
    else if (ay >= ax && ay >= az) {
        m = ay;
        if (n.y < 0.0) { face = 2; uv = vec2( n.x,  n.z); }  // +Y Up
        else           { face = 3; uv = vec2( -n.z, n.x); }  // -Y Down
    }
    else {
        m = az;
        if (n.z > 0.0) { face = 0; uv = vec2( -n.x, n.y); }  // +Z Back
        else           { face = 5; uv = vec2( n.x, n.y); }  // -Z Front
    }

    uv = uv / m * 0.5 + 0.5;

    float f = float(face);
    float col = f - 2.0 * floor(f * 0.5);
    float row = floor(f * 0.5);

    vec2 atlasUV = (uv + vec2(col, row)) / vec2(2.0, 3.0);

    return texture2D(atlas, atlasUV);
}

vec3 ApplyPuddleEffect(
    vec3 diffuseColor,
    vec3 viewPos,
    vec3 viewDir,
    vec3 N,
    vec3 sunDir,
    vec3 sunColor,
    vec3 FogColor,
    vec2 pos_xz,
    float rain,
    float snow,
    float night,
    float dawn,
    float time,
    float dither,
    bool water,
    bool doEffect,
    bool env_underwater,

    sampler2D s_Puddles,
    sampler2D s_SSRTex,
    sampler2D s_NoiseVoxel,
    vec3 position 
){
#if defined(PUDDLES)
    if (doEffect && rain > 0.5) {
        float NdotUp = max(N.y, 0.0);
        if (NdotUp <= 0.001) return diffuseColor; // quick reject for vertical surfaces

        // --- Fresnel & reflection direction ---
        float fresnelVal = fresnelSchlick(viewPos, N, 0.2);
        vec3 viewDir_reflect = reflect(viewDir, N);

        // --- Puddle mask ---
        float puddleMask = 1.0;
        #if defined(SHOW_REFLECTIONS_ON_PUDDLES_ONLY)
            puddleMask = 1.0 - texture2D(s_Puddles, position.xz * 0.00925).r;
        #endif
        puddleMask *= NdotUp;
        float edgeFade = smoothstep(0.0, 0.2, puddleMask);

        // --- Material glossiness ---
        float gloss = mix(0.7, 0.99, puddleMask); // 1.0 - roughness

        // --- Sky reflection approximation ---
        vec3 rainSky = vec3_splat(0.7);
        vec3 skyReflect = rainSky * mix(0.5, 1.0, fresnelVal);

        // --- Screen-space reflection ---
        vec4 reflection = vec4(0.0);
        #if !defined(ALPHA_TEST)
            vec4 ssr = textureCube2x3(s_SSRTex, -viewDir_reflect);
            ssr.rgb = pow(ssr.rgb, vec3(2.0 + 2.0 * night * (1.0 - dawn)));
            ssr.a *= 0.7;
            reflection = ssr;
        #endif

        // --- Blend SSR with sky fallback ---
        reflection.rgb = mix(reflection.rgb, skyReflect, gloss);

        // --- Base color adjustment under wetness ---
        vec3 puddleColor = diffuseColor * 0.4 + rainSky * 0.2;
        vec3 baseColor = mix(diffuseColor, puddleColor, puddleMask);

        // --- Aurora glow reflection ---
        vec3 aurora = GetAurora(viewDir_reflect, time, dither, s_NoiseVoxel);
        baseColor += aurora * night * (1.0 - dawn) * puddleMask * fresnelVal;

        // --- Wetness effect ---
        float wetness = rain * mix(0.8, 1.0, puddleMask);

        // --- Combine reflection and diffuse ---
        vec3 rainTerrain = mix(
            baseColor,
            reflection.rgb * reflection.a,
            fresnelVal * wetness * edgeFade
        );

        // --- Specular ---
        // Optionally re-enable specular highlight
        // vec3 rainSpec = specularHighlights(viewPos, sunDir, dot(N, sunDir), sunColor, N, 0.01 * gloss);

        // --- Final blend ---
        if (!water && !env_underwater) {
            diffuseColor = mix(diffuseColor, rainTerrain, rain * (1.0 - snow));
            // diffuseColor += rainSpec * puddleMask * rain;
        }
    }
#endif
    return diffuseColor;
}


float getWaveHeightGest(vec2 uv, float t) {
const int N = WAVE_COUNT;        // number of waves
float h = 0.0;
float totalWeight = 0.0;

// global parameters  
float timeScale = 0.5;  // wave speed factor  
float steepness = 2.0;  // wave sharpness (crestiness)  

for (int i = 0; i < N; i++) {  
    // Spread directions evenly around a circle with a slight random offset  
    float angle = float(i) * (6.28318 / float(N)) + sin(float(i) * 1.91) * 0.1;  
    vec2 dir = normalize(vec2(cos(angle), sin(angle)));  

    // Frequency and amplitude (spectrum)  
    float freq = 0.2 + float(i) * 0.15;  
    float amp  = 0.6 / (1.0 + float(i) * 0.3);  // decreasing amplitude  
    float speed = sqrt(9.8 * freq);             // simple deep-water dispersion relation  

    // Random phase offset  
    float phase = sin(float(i) * 3.17) * 6.2831;  

    // Gerstner wave core formula  
    float wave = sin(dot(dir, uv * freq) + speed * t * timeScale + phase);  
    float crest = cos(dot(dir, uv * freq) + speed * t * timeScale + phase);  

    // Add height contribution  
    h += wave * amp;  
    totalWeight += amp;  

    // Optional: displace UVs for “rolling” motion (if you want later)  
     uv += dir * crest * amp * steepness;  
}  

// Normalize by total amplitude  
h /= totalWeight;  

// Scale final wave height  
return h*0.25;

}// =======================================
/*vec3 getWaterNormalGest(vec2 uv, float t) {
float eps = 0.02;
float scale = 7.0;
float h  = getWaveHeightGest(uv, t);
float hx = getWaveHeightGest(uv + vec2(eps, 0.0), t);
float hy = getWaveHeightGest(uv + vec2(0.0, eps), t);

float dx = (hx - h) / eps;  
float dy = (hy - h) / eps;  

return normalize(vec3(-dx,1.0 ,-dy));

}*/

vec3 getWaterNormalGest(vec2 uv, float t) {
    float eps = 0.1;   // larger epsilon gives more stable gradients
    float invEps = 1.0 / eps;

    // Sample heights in a 3x3 pattern (central difference for accuracy)
    float hL = getWaveHeightGest(uv - vec2(eps, 0.0), t);
    float hR = getWaveHeightGest(uv + vec2(eps, 0.0), t);
    float hD = getWaveHeightGest(uv - vec2(0.0, eps), t);
    float hU = getWaveHeightGest(uv + vec2(0.0, eps), t);

    // Compute central differences
    float dx = (hR - hL) * 0.5 * invEps;
    float dy = (hU - hD) * 0.5 * invEps;

    // Construct and normalize normal
    vec3 n = normalize(vec3(dx, 1.0, dy));
    return n;
}
vec4 getWaterForGesterner(vec2 uv, vec2 resolution, float scale, float time) {
vec2 step = 1.0 / resolution;
float height = getWaveHeight(uv, time);

vec2 dxy = height - vec2(  
   getWaveHeight(uv + vec2(step.x, 0.0), time),  
  getWaveHeight(uv + vec2(0.0, step.y), time)  
);  
return vec4(normalize(vec3(dxy * scale / step, 1.0)), height);

}
const float INV_NOISERES = 1.0 / 16.0;
const float Z_STRETCH = 17.0 * INV_NOISERES;

// shortcuts

// ------------------------------------------------------------
// Lightweight 3D noise using the provided 2D texture atlas
// ------------------------------------------------------------
float Get3DNoise(in vec3 pos,sampler2D noise) {
    // pack z into tile offset (fast)
    float pz = floor(pos.z);
    float fz = pos.z - pz;
    vec2 base = pos.xy * INV_NOISERES + pz * Z_STRETCH;
    // two fetches (can't avoid atlas interpolation without changing texture)
    float a = texture2D(noise, base).x;
    float b = texture2D(noise, base + vec2(Z_STRETCH)).x;
    return mix(a, b, fz);
}
float phaseHG(float cosTheta, float g) {
    float g2 = g * g;
    return 0.25 * (1.0 / 3.14159265) * (1.0 - g2) * pow(1.0 + g2 - 2.0 * g * cosTheta, -1.5);
}
#define pi radians(180.0)
#define hpi (pi / 2.0)
#define tau (pi * 2.0)
#define phi (0.5*sqrt(5.0) + 0.5)

float fbmCloud(vec3 position,int k,sampler2D noise) {
  const int steps = 3;
  float results = 0.0, density = 0.5;

  for (int i = 0; i < steps; i++) {
    results += density * Get3DNoise(position,noise);
    position = position*3.0;
    density *= 0.5;
  }

  return clamp(results, 0.0, 1.0) + 1.0/tau/float(steps);
}
float cloudDensity(vec3 noisePos,sampler2D noise){
              float base = fbmCloud(noisePos, 3,noise);

        float density = base;
        density = 1.8 * clamp(density - 0.4, 0.0, 1.0);
      return  density = pow(density, 3.1) /* heightFactor*/;
      }
    
      vec4 stylizedVLClouds(vec3 rayOrigin, vec3 viewDir, float time, float jitter, vec3 sunDir, vec3 sky,vec3 sunCol,int k,sampler2D noise) {
  float cloudBase = 1.2;
    float cloudTop = 2.0;
    int steps = 5;
    float stepSize = (cloudTop - cloudBase) / float(steps);

    vec3 cloudAccum = vec3(0.0);
    float alphaAccum = 0.0;
    float viewLift = smoothstep(0.1, 0.2, viewDir.y);

    for (int i = 0; i < steps; i++) {
        float height = cloudBase + stepSize * (float(i) + jitter);
        float t = height / max(viewDir.y, 0.001);
        vec3 pos = rayOrigin + viewDir * t;

        vec3 noisePos = vec3(pos.xz * 0.1 + time * 0.05, height * 0.5);
        float base = fbm(noisePos, 3);

        float heightNorm = (height - cloudBase) / (cloudTop - cloudBase);
        float heightFactor = smoothstep(0.0, 0.2, heightNorm) * (1.0 - smoothstep(0.6, 1.0, heightNorm));


      float density =cloudDensity(noisePos,noise);
      float phase = phaseHG(dot(viewDir,sunDir),0.53);
        float alpha = 1.0 - exp(-density * 3.0);
        alpha *= (1.0 - alphaAccum) * viewLift;

        // --- NEW SCATTERING & BASE DARKENING ---

        float scattering = clamp(dot(sunDir, vec3(0.0, 1.0, 0.0)), 0.0, 1.0);
        scattering = mix(0.2, 1.0, heightNorm); // darker near base, brighter at top
  vec3 cloudColor = sunCol*exp(-scattering*5.0) + phase *sunCol;
       // cloudColor += sunCol * phase * density * 0.2;


        cloudAccum += cloudColor* alpha;
        alphaAccum += alpha;
        if (alphaAccum > 0.98 && viewDir.y < 0.9) break;
    }

    vec4 clouds = vec4(pow(cloudAccum,vec3(1.0/1.9)), alphaAccum);
return clouds;
}

vec3 ApplyWaterEffect(
vec3 diffuseColor,
in vec3 N,
vec3 viewDir,
vec3 viewPos,
vec3 v_position,
vec2 v_texcoord0,
vec3 sunDir,
vec3 FogColor,
vec3 rayOrigin,
float day,
float night,
float rain,
float dusk,
float dawn,
float snow,
float time,
float jitter,
float dither,
bool water,

// --- Textures ---  
sampler2D s_NormalsTex,  
sampler2D s_MatTexture,  
sampler2D s_SunTex,  
sampler2D s_SSRTex,  
sampler2D s_NoiseVoxel,  
vec3 waterPosition,
sampler2D noiseTex,
sampler2D s_MoonTex,
vec2 fragTexCoord,
vec3 waterCol,
vec3 absorption,
float depth,
out vec3 waterNormal
){
#if defined(ENABLE_WATER)
if (water) {
vec3 skySunCol;

#if defined(WAVY_WATER)  
  N = getWaterNormalGest(waterPosition.xz*10.0,time);  

#else  
    N = normalize(mul(getTBN(N,fragTexCoord,v_position), getNormal(s_MatTexture, v_texcoord0)));  
#endif

//  N = flatN;
waterNormal = N;
float ndotl_water = max(dot(N, viewPos), 0.0);  
    vec3 viewDir_refl = reflect(viewDir, N);  

vec3 skyColor = mainSkyRender(viewDir_refl, sunDir, day, night, rain, dusk, dawn, snow, skySunCol,time);
#if !defined(WAVY_WATER)  
    waterCol = diffuseColor;  
    waterCol *= max(dot(N,sunDir),0.0);
#else  
    waterCol *= max(dot(N,sunDir),0.0);    
#endif  
      float fresnel = fresnelSchlick(viewPos, N, 0.02);

#if !defined(WAVY_WATER)
fresnel = 0.9;
#endif

    vec3 aurora = GetAurora(viewDir_refl, time, dither,s_NoiseVoxel);  
    skyColor += ((aurora * night) * (1.0 - dawn))* 0.9*mix(0.7,0.9, fresnel);

float mask;
vec3 sunTex = sunTextureMovement(sunDir, viewDir_refl, mask, s_SunTex);
vec3 moonTex = moonTextureMovement(normalize (vec3(-0.5,0.2,0.0)), viewDir_refl, mask, s_MoonTex);


#if !defined(WAVY_WATER) && defined(VANILLA_SUN)
waterCol *= 0.5 - 0.25 *dawn;
#endif

#if defined(VANILLA_SUN)
if(night>0.8){
skyColor += moonTex;
}else{
skyColor += sunTex * skySunCol;
}
#endif

vec4 ssr = textureCube2x3(s_SSRTex,-viewDir_refl);
ssr.rgb = pow(ssr.rgb,vec3(2.2));


#if!defined(WAVY_WATER)
ssr.a *= fresnel;
#else
ssr.a *= 0.75;
#endif

float strengthOfReflections = fresnel ;
vec4 clouds= fastclouds(viewDir_refl,time,sunDir,skyColor,skySunCol,rain);
     
skyColor += clouds.rgb*clouds.a*mix(0.7,0.9, fresnel);
 
vec3 environment = mix(skyColor,ssr.rgb,ssr.a);
environment *= absorption;
//waterCol = mix(waterCol, clouds.rgb, clouds.a*fresnel);
diffuseColor = mix(waterCol,environment, strengthOfReflections);

    
    #if defined(SPECULAR_HIGHLIGHTS)
    // Specular BRDF lighting on water  
    vec3 specularW = brdf(sunDir, viewPos, 0.2, N, waterCol, 0.0, vec3_splat(0.04), skySunCol);  
    diffuseColor += specularW * fresnel;  
    #endif
    
}

#endif
return diffuseColor;
}

vec3 ApplyCaustics(
    vec3 diffuseColor,
    vec3 N,
    vec3 zenithsky,
    vec2 pos_xz,
    vec2 v_lightmapUV,
    vec3 v_position,
    float time,
    bool env_underwater,
    bool nether,

    // --- Textures ---
    sampler2D s_Caustics,
    sampler2D s_VANILLACaustics,
    vec3 viewDir,
    vec3 waterC
){
    bool blockUnderWater = (
        v_lightmapUV.y < 0.9 &&
        abs((2.0 * v_position.y - 15.0) / 16.0 - v_lightmapUV.y) < 0.00002
    );
    if ((env_underwater || blockUnderWater) && !nether) {
  vec3 watercol = waterC;
     

#if defined(REALISTIC_CAUSTICS)
         vec2 uv = pos_xz * 0.15;
       
        vec2 distort = uv + N.xy;
       // uv = distort;
        //uv += vec2(time * 0.02, time * 0.02);
        //uv += N.xz * 0.05;

        vec3 caustic = texture2D(s_Caustics, uv).rgb;
        float ndotl_c = max(N.y, 0.0);
        caustic *= ndotl_c * 0.5;

        vec3 finalColor = (watercol )+caustic * 6.0;

#else
        vec2 uv = pos_xz;
        uv += N.xz * 0.05;

        vec3 caustic = texture2D(s_VANILLACaustics, fract(uv)).rgb;
        float ndotl_c = max(N.y, 0.0);
        caustic *= ndotl_c * 0.5;

        vec3 finalColor = watercol*0.8 + zenithsky * 0.1 + caustic * 9.0;

#endif

      diffuseColor *= finalColor * 2.7;
    }
    
    return diffuseColor;
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

    // --- Textures ---
    sampler2D s_SunTex,
    sampler2D s_NoiseVoxel
){
#if defined(ENABLE_PBR)
    if (doEffect && (isNonMetal || isMetal)) {

        // Reflection direction and sky base
        vec3 viewDir_reflect = reflect(viewDir, N);
        vec3 skyReflect = storeSky;

        // Add aurora glow at night
        vec3 aurora = GetAurora(viewDir_reflect, time, dither,s_NoiseVoxel);
        skyReflect += (aurora * night) * (1.0 - dawn);

    // Sample clouds (two modes)
     /*   vec4 clouds = REALClouds(
            viewPos, viewDir_reflect, time, jitter,
            sunDir, skyReflect, skySunCol, 5, rain
        );
        clouds = fastclouds(viewDir_reflect, time, sunDir, skyReflect, skySunCol);
*/
        // Sample sun texture
        float mask;
        vec3 sunTex = sunTextureMovement(sunDir, viewDir_reflect, mask, s_SunTex);
        // skyReflect += sunTex * skySunCol; // uncomment if you want sun bloom in reflection

        // Adjust albedo based on Fresnel
        albedo *= 1.0 - F0;;
        albedo =mix(albedo,albedo*skyReflect,diffuseColor.a*fresnel);
        // Specular BRDF term
        vec3 specular = brdf_specular(sunDir, viewPos, N, roughness, F0, skySunCol);

        // Final PBR color (diffuse + specular)
        vec3 pbr = diffuseLight + specular;
        diffuseColor.rgb = albedo * pbr;
    }
    return diffuseColor.rgb;
   #else
   return diffuseColor.rgb;
   #endif
}

float noisegod(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);

    // Interpolation curve
    vec2 u = f * f * (3.0 - 2.0 * f);

    // Noise at corners
    float a = hash12(i + vec2(0.0, 0.0));
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));

    // Bilinear interpolation
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}
float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);

    // Interpolation curve
    vec2 u = f * f * (3.0 - 2.0 * f);

    // Noise at corners
    float a = hash12(i + vec2(0.0, 0.0));
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));

    // Bilinear interpolation
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}
float fbm(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;

    for (int i = 0; i < 5; i++) {  // 5 octaves
        value += amplitude * noisegod(p * frequency);
        frequency *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

// Smooth flash function (randomized)
float lightningFlash(float t) {
    float flashFreq = 0.15; // how often lightning happens
    float phase = fract(t * flashFreq);
    
    // Random start times
    float seed = floor(t * flashFreq);
    float r = fract(sin(seed * 43758.5453) * 12.9898);

    // Only trigger flash sometimes
    float trigger = step(0.9, r); 

    // Flash curve (quick rise, fast decay)
    float flash = exp(-20.0 * phase) * trigger;
    
    return flash;
}
vec3 applyFog(vec3 color, vec3 worldPos, vec3 cameraPos, vec3 sunDir, vec3 sunColor, vec3 fogColor, float fogDensity, float heightFalloff)
{
    // Distance from camera to fragment
    float dist = length(worldPos - cameraPos);
    
    // Height-based exponential density
    float height = worldPos.y;
    float cameraHeight = cameraPos.y;
    float avgHeight = (height + cameraHeight) * 0.5;
    float density = fogDensity * exp(-avgHeight * heightFalloff);
    
    // Transmittance using Beer-Lambert law
    float fogAmount = 1.0 - exp(-dist * density);
    
    // Optional: light scattering from sun
    float sunScatter = max(dot(normalize(worldPos - cameraPos), -sunDir), 0.0);
    sunScatter = pow(sunScatter, 8.0); // make it forward-scattering
    vec3 scattering = sunColor * sunScatter * 0.4;
    
    // Combine
    vec3 finalFogColor = fogColor + scattering;
    return mix(color, finalFogColor, fogAmount);
}
// Simple 3D noise function for fog
float hashfog(vec3 p) {
    p = fract(p * 0.3183099 + 0.1);
    p *= 17.0;
    return fract(p.x * p.y * p.z * (p.x + p.y + p.z));
}

float noise3Dfog(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f); // smoothstep interpolation
    
    return mix(
        mix(mix(hashfog(i + vec3(0,0,0)), hashfog(i + vec3(1,0,0)), f.x),
            mix(hashfog(i + vec3(0,1,0)), hashfog(i + vec3(1,1,0)), f.x), f.y),
        mix(mix(hashfog(i + vec3(0,0,1)), hashfog(i + vec3(1,0,1)), f.x),
            mix(hashfog(i + vec3(0,1,1)), hashfog(i + vec3(1,1,1)), f.x), f.y),
        f.z
    );
}

// Fractal Brownian Motion for more detailed noise
float fbmfog(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;
    
    // 3 octaves for performance (add more for detail)
    for(int i = 0; i < 3; i++) {
        value += amplitude * noise3Dfog(p * frequency);
        frequency *= 2.0;
        amplitude *= 0.5;
    }
    
    return value;
}

float getLayeredFog(vec3 worldPos, vec3 cameraPos, vec2 fogHeights, vec2 fogDensity, float dist) {
    // fogHeights.x = bottom layer height
    // fogHeights.y = top layer height
    // fogDensity.x = near layer density
    // fogDensity.y = upper layer density

    float heightFactor = clamp((worldPos.y - fogHeights.x) / (fogHeights.y - fogHeights.x), 0.0, 1.0);
    float camHeightFactor = clamp((cameraPos.y - fogHeights.x) / (fogHeights.y - fogHeights.x), 0.0, 1.0);
    float density = mix(fogDensity.x, fogDensity.y, heightFactor);
    float camDensity = mix(fogDensity.x, fogDensity.y, camHeightFactor);
    float layerVisibility = 1.0 - smoothstep(0.0, 1.0, camHeightFactor);
    float fogFactor = 1.0 - exp(-dist * (density + camDensity) * 0.5);
    fogFactor *= layerVisibility;
    return clamp(fogFactor, 0.0, 1.0);
}

float fogFactor(float relativeDist, vec3 fogPos, vec2 FOG_CONTROL, float time) {
#ifdef NL_FOG
    // ============ Configuration ============
    float fogNear    = FOG_CONTROL.x * 0.5;
    float fogFar     = FOG_CONTROL.y * 1.2;
    float baseDensity = 0.35;
    
    // Noise parameters
    float noiseScale  = 0.002;
    float noiseDetail = 0.008;
    float timeSpeed   = 0.03;
    
    // Height fog parameters
    float groundLevel = 64.0;
    float heightFalloff = 0.025;
    float heightScale = 1.5;
    
    // ============ 1. Multi-Octave Noise ============
    // Animated world position
    vec3 animPos = fogPos * noiseScale + vec3(time * timeSpeed, time * timeSpeed * 0.3, 0.0);
    
    // Large-scale fog structures (base layer)
    float noise1 = noise3Dfog(animPos);
    
    // Medium detail
    float noise2 = noise3Dfog(animPos * 2.5 + vec3(0.0, time * timeSpeed * 0.5, 0.0));
    
    // Fine detail
    float noise3 = noise3Dfog(animPos * 6.0);
    
    // Combine octaves with decreasing influence
    float combinedNoise = noise1 * 0.5 + 
                          noise2 * 0.3 + 
                          noise3 * 0.2;
    
    // Remap to create more variation (0.3 to 1.1 range)
    combinedNoise = combinedNoise * 0.8 + 0.3;
    
    // ============ 2. Height-Based Density ============
    // Ground-hugging fog that dissipates with altitude
    float heightAboveGround = max(fogPos.y - groundLevel, 0.0);
    float heightDensity = exp(-heightAboveGround * heightFalloff) * heightScale;
    
    // Add subtle noise to height transition
    float heightNoise = noise3Dfog(fogPos * 0.001) * 0.3 + 0.7;
    heightDensity *= heightNoise;
    
    // ============ 3. Distance Fog Calculation ============
    // Smooth distance transition
    float distFade = smoothstep(fogNear, fogFar, relativeDist);
    
    // Non-linear distance response for better depth perception
    float distCurve = pow(distFade, 1.3);
    
    // ============ 4. Volumetric Density ============
    // Exponential fog with noise modulation
    float density = baseDensity * combinedNoise * (1.0 + heightDensity * 0.5);
    float volumetricFog = 1.0 - exp(-relativeDist * density * 0.01);
    
    // ============ 5. Directional Variation ============
    // Add directional bias (wind effect)
    float windDirection = noise3Dfog(vec3(time * 0.02, 0.0, 0.0)) * 2.0 - 1.0;
    float directionalMod = 1.0 + sin(fogPos.x * 0.001 + windDirection) * 0.15;
    
    // ============ 6. Final Composition ============
    float finalFog = volumetricFog * distCurve * directionalMod;
    
    // Apply height density boost for ground fog
    finalFog = mix(finalFog, min(finalFog * (1.0 + heightDensity), 1.0), 0.6);
    
    // Add subtle distance-based noise to break up uniformity
    float distanceNoise = noise3Dfog(fogPos * 0.0005 + vec3(time * 0.01)) * 0.1;
    finalFog += distanceNoise * distFade;
    
    // Ensure valid range with smooth clamping
    finalFog = clamp(finalFog, 0.0, 0.95);
    
    // Optional: Add atmospheric perspective (more fog = more blue shift)
    // This multiplier can be used in your color mixing
    // float atmosphericDepth = pow(finalFog, 0.7);
    
    return finalFog;
#else
    return 0.0;
#endif
}

// ============================================
// UNDERWATER COLOR ABSORPTION
// ============================================

vec3 underwaterAbsorption(vec3 color, float depth, vec3 worldPos, float time) {
    // === Absorption coefficients (how quickly each channel fades) ===
    // Red absorbs fastest, blue slowest (realistic water physics)
    vec3 absorptionCoeffs = vec3(
        0.45,  // Red - absorbs very quickly
        0.15,  // Green - absorbs moderately
        0.05   // Blue - absorbs slowly
    );
    
    // === 1. Exponential Absorption (Beer-Lambert Law) ===
    // Each color channel fades exponentially with depth
    vec3 absorption = exp(-absorptionCoeffs * depth);
    vec3 absorbedColor = color * absorption;
    
    // === 2. Water Tint/Fog Color ===
    // Deep ocean blue-green color that replaces absorbed light
    vec3 waterColor = vec3(0.02, 0.15, 0.35); // Deep blue-green
    // Tropical/shallow water alternative:
    // vec3 waterColor = vec3(0.05, 0.25, 0.45); // Lighter cyan-blue
    
    // === 3. Scattering (how much water color mixes in) ===
    float scatterAmount = 1.0 - exp(-depth * 0.08);
    vec3 scattered = mix(absorbedColor, waterColor, scatterAmount);
    
    // === 4. Depth-based Darkness ===
    // Deep water gets progressively darker
    float depthDarkening = exp(-depth * 0.03);
    scattered *= depthDarkening;
    
    // === 5. Particle Scatter Highlights ===
    // Subtle bright spots from suspended particles catching light
    float particleNoise = noise3Dfog(worldPos * 2.0 + vec3(time * 0.1));
    particleNoise = pow(max(particleNoise - 0.7, 0.0), 2.0) * 0.08;
    
    // Particles fade with depth
    float particleStrength = exp(-depth * 0.2);
    scattered += vec3(particleNoise * particleStrength);
    
    // === 6. Murkiness/Turbidity (particle suspension) ===
    #ifdef UNDERWATER_MURKY
    float turbidity = 0.3; // 0.0 = clear, 1.0 = very murky
    vec3 murkyColor = vec3(0.08, 0.1, 0.08); // Greenish-brown particles
    float murkiness = (1.0 - exp(-depth * turbidity * 0.5)) * turbidity;
    scattered = mix(scattered, murkyColor, murkiness);
    #endif
    
    return scattered;
}

// ============================================
// UNDERWATER FOG FACTOR
// ============================================

float underwaterFogFactor(float distance, float depthBelowSurface) {
    // Visibility decreases with both distance and depth
    
    // === Base fog parameters ===
    float fogDensity = 0.08;  // How thick the water is
    float depthInfluence = 0.02; // How much depth affects fog
    
    // === Distance-based fog ===
    float distanceFog = 1.0 - exp(-distance * fogDensity);
    
    // === Depth makes fog stronger ===
    float depthFactor = 1.0 + (depthBelowSurface * depthInfluence);
    
    // === Combined fog ===
    float fog = clamp(distanceFog * depthFactor, 0.0, 0.95);
    
    return fog;
}

// ============================================
// COMPLETE UNDERWATER EFFECT
// ============================================

vec3 applyUnderwaterEffect(vec3 originalColor, vec3 worldPos, vec3 cameraPos, float time) {
    // === Check if underwater ===
    float waterLevel = 63.0; // Minecraft sea level
    float cameraDepth = max(waterLevel - cameraPos.y, 0.0);
    
    if (cameraDepth <= 0.0) {
        return originalColor; // Not underwater
    }
    
    // === Calculate distances ===
    float distanceToPixel = length(worldPos - cameraPos);
    float pixelDepth = max(waterLevel - worldPos.y, 0.0);
    
    // Use average depth for absorption calculation
    float avgDepth = (cameraDepth + pixelDepth) * 0.5;
    
    // === Apply absorption ===
    vec3 absorbed = underwaterAbsorption(originalColor, avgDepth, worldPos, time);
    
    // === Apply fog ===
    float fogAmount = underwaterFogFactor(distanceToPixel, cameraDepth);
    vec3 waterFogColor = vec3(0.02, 0.15, 0.35); // Match absorption water color
    vec3 finalColor = mix(absorbed, waterFogColor, fogAmount);
    
    return finalColor;
}

// ============================================
// SIMPLE INTEGRATION EXAMPLE
// ============================================

// In your main shader:
// vec3 finalColor = applyUnderwaterEffect(baseColor, worldPosition, cameraPosition, gameTime);
//
// Or manual approach:
// if (isUnderwater) {
//     float depth = waterLevel - worldPos.y;
//     finalColor = underwaterAbsorption(finalColor, depth, worldPos, time);
//     
//     float fog = underwaterFogFactor(distance, cameraDepth);
//     finalColor = mix(finalColor, waterColor, fog);
// }

// ============================================================================
// LAYERED FOG IMPLEMENTATION (Based on ogldev)
// ============================================================================

// ============================================================================
// LAYERED FOG CALCULATION FUNCTION
// All inputs are passed as parameters - no uniforms needed in the function!
// ============================================================================

float CalcLayeredFogFactor(
    vec3 cameraWorldPos,    // Camera position in world space
    vec3 pixelWorldPos,     // Fragment/pixel position in world space
    float fogTop,           // Height of fog layer top (e.g., 20.0)
    float fogEnd            // Maximum fog distance (e.g., 100.0)
)
{
    // Project camera and pixel positions onto the horizontal plane (y = 0)
    vec3 CameraProj = cameraWorldPos;
    CameraProj.y = 0.0;
    
    vec3 PixelProj = pixelWorldPos;
    PixelProj.y = 0.0;
    
    // Calculate horizontal distance through fog (normalized)
    float DeltaD = length(CameraProj - PixelProj) / fogEnd;
    
    // Initialize vertical variables
    float DeltaY = 0.0;
    float DensityIntegral = 0.0;
    
    // Case 1: Camera is above the fog layer
    if (cameraWorldPos.y > fogTop) {
        // Check if pixel is inside the fog
        if (pixelWorldPos.y < fogTop) {
            // Ray enters fog from above
            DeltaY = (fogTop - pixelWorldPos.y) / fogTop;
            DensityIntegral = DeltaY * DeltaY * 0.5;
        }
        // else: pixel is also above fog - nothing to do
    } 
    // Case 2: Camera is inside the fog layer
    else {
        // Check if pixel is also inside the fog
        if (pixelWorldPos.y < fogTop) {
            // Both camera and pixel are inside fog
            DeltaY = abs(cameraWorldPos.y - pixelWorldPos.y) / fogTop;
            
            // Calculate density integral from ground to camera
            float DeltaCamera = (fogTop - cameraWorldPos.y) / fogTop;
            float DensityIntegralCamera = DeltaCamera * DeltaCamera * 0.5;
            
            // Calculate density integral from ground to pixel
            float DeltaPixel = (fogTop - pixelWorldPos.y) / fogTop;
            float DensityIntegralPixel = DeltaPixel * DeltaPixel * 0.5;
            
            // Fog between camera and pixel
            DensityIntegral = abs(DensityIntegralCamera - DensityIntegralPixel);
        } 
        else {
            // Camera inside fog, pixel above fog
            DeltaY = (fogTop - cameraWorldPos.y) / fogTop;
            DensityIntegral = DeltaY * DeltaY * 0.5;
        }
    }
    
    // Calculate final fog density
    float FogDensity = 0.0;
    
    if (DeltaY != 0.0) {
        // Calculate actual path length through fog using Pythagorean theorem
        float PathLengthFactor = sqrt(1.0 + ((DeltaD / DeltaY) * (DeltaD / DeltaY)));
        FogDensity = PathLengthFactor * DensityIntegral;
    }
    
    return FogDensity;
}
    
// Mobile-optimized parameters
#define MARCH_STEPS 20
#define MARCH_SIZE 0.7
#define CLOUD_ABSORPTION 6.8
#define LIGHT_SAMPLES 1
#define CLOUD_CENTER_HEIGHT 35.0  // Position clouds above camera
#define CLOUD_SCALE 25.0           // Scale up the cloud volume

//#define u_time time

// ------------------------------------------------------------
// Lightweight 3D noise using the provided 2D texture atlas
// Simplified 3D noise
// Simplified 3D noise
float noisemed(vec3 x) {
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f * f * (3.0 - 2.0 * f);

    float n = p.x + p.y * 157.0 + 113.0 * p.z;
   return mix(
        mix(mix(hash(vec3(n, 0.0, 0.0)), hash(vec3(n + 1.0, 0.0, 0.0)), f.x),
            mix(hash(vec3(n + 157.0, 0.0, 0.0)), hash(vec3(n + 158.0, 0.0, 0.0)), f.x), f.y),
        mix(mix(hash(vec3(n + 113.0, 0.0, 0.0)), hash(vec3(n + 114.0, 0.0, 0.0)), f.x),
            mix(hash(vec3(n + 270.0, 0.0, 0.0)), hash(vec3(n + 271.0, 0.0, 0.0)), f.x), f.y),
        f.z);
       // return Get3DNoise(x);
}
// Optimized FBM with 3 octaves for texture detail
float fbmend(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;
    
    // 3 octaves - good balance of detail and performance
    for(int i = 0; i < 3; i++) {
        value += amplitude * noise(p * frequency);
        amplitude *= 0.5;
        frequency *= 2.0;
    }
    
    return value;
}

// Enhanced spiral vortex cloud density with FBM texture
float stormDensity(vec3 p, float t) {
    float r = length(p.xz);
    float angle = atan(p.z, p.x);
    float height = p.y;

    // Early rejection (scaled bounds)
    if(r > 4.0 || height < -0.5 || height > 3.0) return 0.0;

    // ENHANCED SPIRAL: logarithmic spiral pattern (like real hurricanes)
    // Tighter curl near center, wider at edges
    float spiralRate = 1.2 - r * 0.15; // Spiral gets looser as radius increases
    float spiral = angle - log(r + 0.1) * spiralRate + t * 0.3;

    // Multiple spiral arms (3-4 arms like real hurricanes)
    float armCount = 3.5;
    float arms = sin(spiral * armCount) * 0.5 + 0.5;
    arms = pow(arms, 1.2);

    // Sharp spiral bands
    float spiralBands = smoothstep(0.25, 0.8, arms);
    spiralBands = pow(spiralBands, 0.7); // Sharper bands

    // Eye wall - strongest density ring with more definition
    float eyeWall = smoothstep(0.25, 0.9, r) * smoothstep(3.8, 1.5, r);
    eyeWall = pow(eyeWall, 0.7);

    // Clear eye in center with sharp edge
    float eye = smoothstep(0.6, 0.1, r);

    // Vortex rotation - density follows spiral more tightly
    float vortexAngle = spiral * 2.0;
    float vortexArms = sin(vortexAngle * 2.0) * 0.5 + 0.5;
    vortexArms = pow(vortexArms, 2.0); // Sharp vortex arms
    
    // Tighter spiral near center
    float innerSpiral = smoothstep(2.5, 0.5, r);
    float tightSpiral = sin((spiral + r * 0.3) * (3.0 + innerSpiral * 4.0)) * 0.5 + 0.5;
    tightSpiral = smoothstep(0.35, 0.65, tightSpiral);

    // Combine spiral structures with vortex
    float density = eyeWall * spiralBands * (1.0 - eye * 0.97);
    density *= (0.5 + tightSpiral * 0.3 + vortexArms * 0.2);

    // FBM for realistic cloud texture - rotates with vortex
    vec3 texCoord = p * 2.0;
    texCoord.xz = mat2(cos(r * 0.3), -sin(r * 0.3), 
                       sin(r * 0.3), cos(r * 0.3)) * p.xz * 2.0;
    texCoord += vec3(t * 0.1, 0.0, 0.0);
    
    float cloudTexture = fbmend(texCoord);
    cloudTexture = cloudTexture * 0.6 + 0.4; // Don't let texture remove too much density
    density *= cloudTexture;

    // Height falloff - spiral thickest at mid-height
    float heightFalloff = exp(-abs(height - 0.5) * 1.2);
    heightFalloff *= (0.65 + 0.35 * smoothstep(2.5, 0.3, r));
    density *= heightFalloff;

    // Smooth outer edge fade
    density *= smoothstep(4.0, 2.8, r);

    // Rotating tendrils at outer edge - follow spiral pattern
    float tendrils = sin(spiral * 7.0 - t * 0.6) * 0.5 + 0.5;
    tendrils = pow(tendrils, 1.5);
    tendrils *= smoothstep(3.8, 2.2, r) * smoothstep(1.3, 2.5, r);
    
    // Add wispy trailing arms
    float trailingArms = sin(spiral * 12.0 + t * 0.4) * 0.5 + 0.5;
    trailingArms *= smoothstep(3.5, 2.0, r) * smoothstep(0.8, 2.0, r);
    trailingArms *= 0.15;
    
    density += tendrils * 0.25 + trailingArms;

    // Subtle turbulence variation following rotation
    float turbulence = fbmend(p * 3.0 + vec3(cos(spiral) * 0.5, 0.0, sin(spiral) * 0.5));
    density *= (0.85 + turbulence * 0.15);

    return clamp(density * 0.9, 0.0, 1.0);
}

// Simplified lightning structure
struct LightningBolt {
    vec3 pos;
    float intensity;
};

// Get 2 lightning sources
void getLightning(float t, out LightningBolt lights[2]) {
    float angle1 = t * 0.5;
    float flash1 = exp(-fract(t * 0.18) * 70.0);
    lights[0].pos = vec3(cos(angle1) * 2.0, 1.5, sin(angle1) * 2.0);
    lights[0].intensity = flash1 * 2.5;

    float angle2 = t * 0.6 + 2.0;
    float flash2 = exp(-fract(t * 0.13 + 0.5) * 60.0);
    lights[1].pos = vec3(cos(angle2) * 2.2, 1.8, sin(angle2) * 2.2);
    lights[1].intensity = flash2 * 2.0;
}

// Optimized phase function
float phaseSimple(float cosTheta) {
    return 0.25 + 0.75 * (1.0 + cosTheta * 0.5);
}

// Simplified volumetric lighting
vec3 volumetricLight(vec3 pos, vec3 viewDir, LightningBolt light, float cloudDensity,float u_time, out vec3 LDir) {
    if(light.intensity < 0.01) return vec3(0.0);

    vec3 toLight = light.pos - pos;
    float lightDist = length(toLight);
    vec3 lightDir = toLight / lightDist;
LDir = lightDir;
    // Simple attenuation
    float atten = 1.0 / (1.0 + lightDist * lightDist * 0.2);
    atten *= exp(-lightDist * 0.1);

    // Simplified shadow sampling
    float shadow = 1.0;
  /*  for(int i = 0; i < LIGHT_SAMPLES; i++) {
        float fi = float(i) / float(LIGHT_SAMPLES);
        vec3 samplePos = pos + lightDir * lightDist * fi;
        shadow *= exp(-stormDensity(samplePos, u_time) * 0.35);
    }*/

    // Simple phase
    float cosTheta = dot(viewDir, lightDir);
    float phase = phaseSimple(cosTheta);

    // Direct glow
    float directGlow = exp(-lightDist * 0.8) * light.intensity * 0.5;

    vec3 lightColor = vec3(0.7, 0.85, 1.0);
    vec3 scattered = lightColor * light.intensity * atten * shadow * phase * cloudDensity * 2.5;
    vec3 direct = lightColor * directGlow;

    return scattered + direct;
}

float nsamp(vec2 coord, sampler2D bluen2){
 return texture2D(bluen2, coord/300.2).r;
 }
float getGodRays(vec2 coord, sampler2D bluen2, float time)
{
 float dither;
  dither = texture2D(bluen2,mod(coord,256.0)).r;
 float v = smoothstep(0.5, 0.9, 1.5 * nsamp((time+(coord) * (dither * 1.5) ),bluen2));
return v * (1.0/length(coord) * 3.0);
}
float normalHemisphereAO(vec3 normal, vec3 worldPos) {
    float occlusion = 0.0;
    int samples = 4;
    
    for (int i = 0; i < samples; i++) {
        // Create random hemisphere samples oriented around normal
        vec3 randomVec = normalize(vec3(
            sin(float(i) * 123.456),
            cos(float(i) * 456.789), 
            sin(float(i) * 789.123)
        ));
        
        // Make sure sample is in same hemisphere as normal
        vec3 sampleDir = normalize(randomVec + normal);
        
        // Simple heuristic: more occlusion when normal points away from "up"
        float weight = max(0.0, dot(sampleDir, vec3(0.0, 1.0, 0.0)));
        occlusion += weight;
    }
    
    occlusion /= float(samples);
    return pow(occlusion, 0.5); // Gamma correction
}



// POM Configuration Constants
const float POM_HEIGHT_SCALE = 0.08;
const float POM_MIN_LAYERS = 8.0;
const float POM_MAX_LAYERS = 32.0;
const float POM_RELIEF_BIAS = 0.01;
const float POM_RELIEF_SCALE = 0.98;
const float POM_DEPTH_OFFSET = 0.04;
const int POM_MAX_STEPS = 32;

// Utility functions
float getLuminance(vec3 color) {
    return dot(color, vec3(0.299, 0.587, 0.114));
}

float getHeight(vec2 texCoord,sampler2D uDiffuseTexture) {
    vec4 color = texture2D(uDiffuseTexture, texCoord);
    float height = getLuminance(color.rgb);
    
    // Apply bias and scale for better relief
    height = (height * POM_RELIEF_SCALE) + POM_RELIEF_BIAS;
    return height;
}

vec2 parallaxOcclusionMapping(vec2 texCoord, vec3 viewDirTangent, sampler2D uDiffuseTexture) {
    // Early out for flat surfaces or edge cases
    if (viewDirTangent.z < 0.01) return texCoord;
    
    // Calculate number of layers based on viewing angle
    float numLayers = mix(POM_MAX_LAYERS, POM_MIN_LAYERS, abs(viewDirTangent.z));
    numLayers = clamp(numLayers, POM_MIN_LAYERS, POM_MAX_LAYERS);
    
    // Calculate the size of each layer
    float layerDepth = 1.0 / numLayers;
    
    // Current depth and delta
    float currentLayerDepth = 0.0;
    vec2 deltaTexCoords = viewDirTangent.xy * POM_HEIGHT_SCALE / (viewDirTangent.z * numLayers);
    
    // Initial values
    vec2 currentTexCoords = texCoord;
    float currentDepthValue = getHeight(currentTexCoords,uDiffuseTexture);
    
    // Ray marching
    for (int i = 0; i < POM_MAX_STEPS; i++) {
        // Break if we've exceeded the layer count
        if (float(i) >= numLayers) break;
        
        // Shift texture coordinates and depth
        currentTexCoords -= deltaTexCoords;
        currentLayerDepth += layerDepth;
        
        // Get new depth value
        currentDepthValue = getHeight(currentTexCoords,uDiffuseTexture);
        
        // Check if we're behind the surface
        if (currentLayerDepth >= currentDepthValue) {
            break;
        }
    }
    
    // Parallax Occlusion Mapping - interpolate between steps
    vec2 prevTexCoords = currentTexCoords + deltaTexCoords;
    float prevDepthValue = getHeight(prevTexCoords,uDiffuseTexture) - (currentLayerDepth - layerDepth);
    float currentDelta = currentDepthValue - currentLayerDepth;
    float prevDelta = prevDepthValue - (currentLayerDepth - layerDepth);
    
    // Linear interpolation
    float weight = currentDelta / (currentDelta - prevDelta);
    weight = clamp(weight, 0.0, 1.0);
    
    vec2 finalTexCoords = mix(currentTexCoords, prevTexCoords, weight);
    
    return finalTexCoords;
}

#endif