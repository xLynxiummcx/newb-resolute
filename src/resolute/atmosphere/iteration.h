
  
float hashstars(vec3 p3) {
    p3 = fract(p3 * 0.1031);
    p3 += dot(p3, p3.zyx + 31.32);
    return fract((p3.x + p3.y) * p3.z);
}

float renderStars(const vec3 pos, const float rain) {
    float stars = 0.0;
    vec3 p = floor((pos + 16.0) * 265.0);
    stars = smoothstep(0.9975, 1.0, hashstars(p));
    stars = mix(stars, 0.0, rain); // se apagan con lluvia
    return stars;
}

#define BLACKHOLE_RADIUS 0.06
#define HALO_THICKNESS 0.02
#define HALO_INTENSITY 1.4
#define BLOOM_SPREAD 0.5
#define INNER_BLOOM_BLEND 0.8
#define INNER_HALO_RADIUS 0.045
#define INNER_HALO_THICKNESS 0.0015
#define INNER_HALO_INTENSITY 1.5
#define INNER_HALO_SOFTNESS 2.0
#define BLACKHOLE_SCALE 0.7
#define BLACKHOLE_OFFSET vec2(0.0, 0.5)
#define BLACKHOLE_ZOOM 1.

#define USE_DISK_ACCRETION 1

vec3 customBlackholeSky(vec3 viewDir) {
    float h = clamp(viewDir.y * 0.5 + 0.5, 0.0, 1.0); // de -1 a 1 → 0 a 1
    vec3 bottomColor = vec3(0.0,0.0,0.0) * 6.0; // gris
    vec3 topColor = vec3(0.0,0.0,0.0); // negro
    vec3 skyColor = mix(bottomColor, topColor, pow(h, 1.0));

    // Agregar estrellas
    float rain = 0.0; // podrías usar env.rainFactor si lo deseas dinámico
    float stars = renderStars(viewDir, rain);
    skyColor += vec3(stars, stars, stars); // estrellas blancas

    return skyColor;
}
//newb x rexyMC
vec4 renderBlackhole(vec3 vdir, float t) {
    vec3 sky = customBlackholeSky(vdir);

    vec3 bhPos = normalize(vec3(-4.0, -1.0, -1.5));
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), bhPos));
    vec3 up = cross(bhPos, right);

    vec3 diff = vdir - bhPos;
    float diffLen = length(diff);
    vec3 vd = diffLen > 0.0001 ? normalize(diff) : vec3(0.0,0.0,0.0);

    vec2 uv = vec2(dot(vd, right), dot(vd, up)) * BLACKHOLE_SCALE * BLACKHOLE_ZOOM;
    vec2 uvSafe = clamp(uv, -10.0, 10.0);
    float r = length(uvSafe);

    float haloDist = abs(r - BLACKHOLE_RADIUS);
    float haloBase = max(haloDist / HALO_THICKNESS, 0.0001);
    float halo = exp(-pow(haloBase, BLOOM_SPREAD)) * HALO_INTENSITY;
    vec3 haloTint = mix(vec3(1.2,1.2,1.2), vec3(0.8, 0.9, 1.0), smoothstep(0.0, 1.5, haloDist / HALO_THICKNESS));
    vec3 haloColor = clamp(haloTint * halo, 0.0, 1.5);

    float innerDist = abs(r - INNER_HALO_RADIUS);
    float innerBase = max(innerDist / INNER_HALO_THICKNESS, 0.0001);
    float innerHalo = exp(-pow(innerBase, INNER_HALO_SOFTNESS)) * INNER_HALO_INTENSITY;
    vec3 innerHaloColor = clamp(vec3(1.0,1.0,1.0) * innerHalo, 0.0, 1.5);

    vec3 blackholeColor = (r < BLACKHOLE_RADIUS)
        ? mix(vec3(0.0,0.0,0.0), haloColor, INNER_BLOOM_BLEND) + innerHaloColor
        : haloColor;

    float outerGlow = exp(-pow(r * 8.0, 2.0)) * 0.8;
    vec3 outerGlowColor = clamp(vec3(0.5, 0.7, 1.0) * outerGlow, 0.0, 1.5);

    vec3 accretionDisk = vec3(0.0,0.0,0.0);
#if USE_DISK_ACCRETION == 1
    float discFade = exp(-pow(abs(uvSafe.y) * 60.0, 2.0));
    float discShape = smoothstep(0.15, 0.01, abs(uvSafe.x) - BLACKHOLE_RADIUS * 1.2);
    float disc = clamp(discShape * discFade, 0.0, 1.0);

    float stripe = clamp(sin(uvSafe.x * 150.0 + t * 1.0) * 0.5 + 0.5, 0.0, 1.0);
    vec3 discColor = mix(vec3(1.0, 0.5, 0.1), vec3(1.0, 0.8, 0.3), stripe);

    accretionDisk = clamp(discColor * disc * 1.5, 0.0, 1.5);
#endif

    vec3 jetColor = vec3(0.0,0.0,0.0);
    for (int i = 0; i < 1; i++) {
        float yOffset = (i == 0) ? 0.002 : -0.015;
        float yShape = 1.0 - smoothstep(0.0, 0.02, abs(uvSafe.y - yOffset));
        float xShape = smoothstep(0.15, 0.02, abs(uvSafe.x) - BLACKHOLE_RADIUS * 1.2);
        vec3 color = (i == 0) ? vec3(1.2, 0.5,0.0) : vec3(1.0, 0.6, 0.1);
        jetColor += clamp(color * xShape * yShape * 1.2, 0.0, 1.5);
    }
    float pulse = 0.85 + 0.15 * sin(t * 3.0);
    sky += blackholeColor + innerHaloColor + accretionDisk;
    sky += jetColor ;

    sky = clamp(sky, 0.0, 1.0); // Protección para Mali

    float alpha = smoothstep(BLACKHOLE_RADIUS * 1.5, BLACKHOLE_RADIUS * 0.5, r);
    return vec4(sky, alpha);
}