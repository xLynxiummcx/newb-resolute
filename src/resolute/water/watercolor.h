
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
sampler2D s_MatTexture,  
sampler2D s_SunTex,  
sampler2D s_SSRTex,  
sampler2D s_NoiseVoxel,  
vec3 waterPosition,
sampler2D noiseTex,///noise in the red channel 
sampler2D s_MoonTex,
vec2 fragTexCoord,
vec3 waterCol,
vec3 absorption,
float depth,
out vec3 waterNormal,
sampler2D wave1,
sampler2D wave2,
sampler2D wave3,
bool caves
){
#if defined(ENABLE_WATER)
if (water) {
vec3 skySunCol;

#if defined(WAVY_WATER)  
vec2 uv = waterPosition.xz*0.07;

#if WATER_TYPE == 1
 vec3 nmap1 = (texture2D(wave1,vec2(uv.x-time*0.08,uv.y)).rgb*2.0-1.0).xzy;
 vec3 nmap2 = (texture2D(wave2,vec2(uv.x+time*0.1,uv.y)).rgb*2.0-1.0).xzy;
 vec3 nmap3 = (texture2D(wave3,vec2(uv.x+time*0.03,uv.y-time*0.04)*1.0).rgb*2.0-1.0).xzy;
vec3 nmapk = normalize(nmap1+nmap2+nmap3*1.0);
N = nmapk;
#elif WATER_TYPE == 2
N = getWaterNormalGest(waterPosition.xz*10.0,time);  
#endif

#else  
    N = normalize(mul(getTBN(N,fragTexCoord,v_position), getNormal(s_MatTexture, v_texcoord0)));  
#endif

//  N = flatN;
waterNormal = N;
float ndotl_water = max(dot(N, viewPos), 0.0);  
    vec3 viewDir_refl = reflect(viewDir, N);  

vec3 skyColor = mainSkyRender(viewDir_refl, sunDir, day, night, rain, dusk, dawn, snow, skySunCol,time);
#if !defined(WAVY_WATER)  
    waterCol = diffuseColor.rgb;  
    waterCol *= max(dot(N,normalize(vec3(0.7, 0.3, 0.2))),0.0);
#else  
    waterCol *= max(dot(N,normalize(vec3(0.7, 0.3, 0.2))),0.0);    
#endif  
      float fresnel = fresnelSchlick(viewPos, N, 0.02);

#if !defined(WAVY_WATER)
fresnel = 0.05;
#endif

if(!caves){
    vec3 aurora = GetAurora(viewDir_refl, time, dither,s_NoiseVoxel);  
    skyColor += ((aurora * night) * (1.0 - dawn))* 0.9*mix(0.7,0.9, fresnel);
}

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
ssr.rgb = pow(ssr.rgb,vec3_splat(2.2));


#if!defined(WAVY_WATER)
ssr.a *= fresnel;
#else
ssr.a *= 0.75;
#endif

float strengthOfReflections = fresnel ;

#if WATER_TYPE == 1
vec3 environment = skyColor;
#elif WATER_TYPE == 2
vec3 environment = mix(skyColor,ssr.rgb,ssr.a);
#endif


environment *= absorption;
//waterCol = mix(waterCol, clouds.rgb, clouds.a*fresnel);
if(!caves){
diffuseColor = mix(waterCol,environment, strengthOfReflections);
}
    
    #if defined(SPECULAR_HIGHLIGHTS)
    // Specular BRDF lighting on water  
    vec3 specularW = brdf(sunDir, viewPos, 0.2, N, waterCol, 0.0, vec3_splat(0.04), skySunCol);  
    diffuseColor += specularW * fresnel;  
    #endif
    
}
return diffuseColor;
#else
return diffuseColor;
#endif
}

