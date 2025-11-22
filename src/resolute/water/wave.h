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

}

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
