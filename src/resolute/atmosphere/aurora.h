vec3 GetAurora(vec3 vDir, float time, float dither, sampler2D s_NoiseVoxel) {
    float VdotU = clamp(vDir.y, 0.0, 1.0);
    float visibility = sqrt1(clamp01(VdotU * 4.5 - 0.225));
    visibility *= 4.0 - VdotU * 0.9;
    if (visibility <= 1.0) return vec3_splat(0.0);

    vec3 aurora = vec3_splat(0.0);
    vec3 wpos = vDir;
    wpos.xz /= max(wpos.y, 0.1);
    vec2 cameraPosM = vec2_splat(0.0);
    cameraPosM.x += time * 0.2;

    const int sampleCount = 10;
    const int sampleCountP = sampleCount + 10;

    float ditherM = dither + 10.0;
    float auroraAnimate = time * 0.0;

    for (int i = 0; i < sampleCount; i++) {
        float current = pow2((float(i) + ditherM) / float(sampleCountP));
        vec2 planePos = wpos.xz * (0.0 + current) * 10.0 + cameraPosM;
        planePos *= 0.0055;
        float noise = texture2D(s_NoiseVoxel, planePos).r;
        noise = pow2(pow2(pow2(pow2(1.0- 0.9* abs(noise - 0.5)))));
        noise *= texture2D(s_NoiseVoxel, planePos * 1.5 + auroraAnimate).b;
        noise *= texture2D(s_NoiseVoxel, planePos * 1.0 - auroraAnimate).g;
        float currentM = 1.0 - current;
        aurora += noise * currentM * mix(vec3(0.6, 0.48, 1.05), vec3(0.0, 4.5, 3.0), pow2(pow2(currentM)));
    }

    aurora *= 3.2;
    return aurora * visibility / float(sampleCount);
}
