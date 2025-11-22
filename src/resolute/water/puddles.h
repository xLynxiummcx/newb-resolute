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

    sampler2D s_Puddles,//puddles are in the blue channel 
    sampler2D s_SSRTex,
    sampler2D s_NoiseVoxel,
    vec3 position 
){
#if defined(PUDDLES)
    if (doEffect && rain > 0.5) {
        float NdotUp = max(N.y, 0.0);
        if (NdotUp <= 0.001) return diffuseColor; // quick reject for vertical surfaces

        float fresnelVal = fresnelSchlick(viewPos, N, 0.2);
        vec3 viewDir_reflect = reflect(viewDir, N);

        float puddleMask = 1.0;
        #if defined(SHOW_REFLECTIONS_ON_PUDDLES_ONLY)
            puddleMask = 1.0 - texture2D(s_Puddles, position.xz * 0.00925).b;
        #endif
        puddleMask *= NdotUp;
        float edgeFade = smoothstep(0.0, 0.2, puddleMask);

        float gloss = mix(0.7, 0.99, puddleMask); // 1.0 - roughness

        vec3 rainSky = vec3_splat(0.7);
        vec3 skyReflect = rainSky * mix(0.5, 1.0, fresnelVal);

        vec4 reflection = vec4_splat(0.0);
        #if !defined(ALPHA_TEST)
            vec4 ssr = textureCube2x3(s_SSRTex, -viewDir_reflect);
            ssr.rgb = pow(ssr.rgb, vec3_splat(2.0 + 2.0 * night * (1.0 - dawn)));
            ssr.a *= 0.7;
            reflection = ssr;
        #endif

        reflection.rgb = mix(reflection.rgb, skyReflect, gloss);

        vec3 puddleColor = diffuseColor * 0.4 + rainSky * 0.2;
        vec3 baseColor = mix(diffuseColor, puddleColor, puddleMask);

        vec3 aurora = GetAurora(viewDir_reflect, time, dither, s_NoiseVoxel);
        baseColor += aurora * night * (1.0 - dawn) * puddleMask * fresnelVal;

        float wetness = rain * mix(0.8, 1.0, puddleMask);

        vec3 rainTerrain = mix(
            baseColor,
            reflection.rgb * reflection.a,
            fresnelVal * wetness * edgeFade
        );

        if (!water && !env_underwater) {
            diffuseColor = mix(diffuseColor, rainTerrain, rain * (1.0 - snow));
        }
    }
#endif
    return diffuseColor;
}
