
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
    sampler2D s_Caustics,//caustics in the green channel 
    vec3 viewDir,
    vec3 waterC
){
    bool blockUnderWater = (
        v_lightmapUV.y < 0.9 &&
        abs((2.0 * v_position.y - 15.0) / 16.0 - v_lightmapUV.y) < 0.00002
    );
#if defined(REALISTIC_CAUSTICS)
    
    if ((env_underwater || blockUnderWater) && !nether) {
  vec3 watercol = waterC;
     

         vec2 uv = pos_xz * 0.15;
       
        vec2 distort = uv + N.xy;
       // uv = distort;
        //uv += vec2(time * 0.02, time * 0.02);
        //uv += N.xz * 0.05;

        vec3 caustic = texture2D(s_Caustics, uv).ggg;
        float ndotl_c = max(N.y, 0.0);
        caustic *= ndotl_c * 0.5;

        vec3 finalColor = (watercol )+caustic * 6.0;

      diffuseColor *= finalColor * 2.7;
    
}
#endif
    return diffuseColor;
}
