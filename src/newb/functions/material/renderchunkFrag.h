
//RenderChunk/fragment.sc
#if defined(DEPTH_ONLY_OPAQUE) || defined(DEPTH_ONLY) || defined(INSTANCING)
    gl_FragColor = vec4_splat(1.0);
    return;
#endif
 float time = ViewPositionAndTime.w;

    vec3 cameraPos = GameCameraPos.xyz;
    vec2 lm = v_lightmapUV;
    float lm_y = lm.y;
    float weather = GameWeatherID.x;
    nl_environment env = nlDetectEnvironment(FogColor.rgb, FogAndDistanceControl.xyz);
  
    float clear = 1.0 - smoothstep(0.0, 0.5, weather);
    float rain  = env.rainFactor;
    float snow  = smoothstep(1.5, 2.0, weather);

    vec3 whatWeather = getWeather(weather);
    vec4 whatTime = timeofday(GameTimeOfDay.x);
    float night = whatTime.x;
    float day   = whatTime.w;
    float dusk  = whatTime.z;
    float dawn  = whatTime.y;

    bool water = v_extra.b > 0.9;

    vec3 sunDir = normalize(GameSunDir.xyz);
         sunDir = mix(sunDir,normalize(vec3(-0.5,0.2,0.0)),night * (1.0 - dawn) * (1.0 - dusk));
  
    vec3 viewPos = normalize( - v_wpos);    // used across lighting
    vec3 viewDir = normalize(v_wpos);   
    float width = 0.88;
   float shadows = smoothstep(0.9, 0.8, pow(v_lightmapUV.y, 2.0));

    float sideshadow = smoothstep(0.64, 0.62, v_color1.g);
    float sunVisibility = clamp(sunDir.y, 0.0, 1.0);
    float moonVisibility = clamp(-sunDir.y, 0.0, 1.0);
    float nolight = 1.0 - lm_y;
    bool isCave = nolight>0.7;
    vec2 texuv = v_texcoord0;
    vec2 pos_xz = v_position.xz;
  bool nether= env.nether;
 
   vec3 N = normalize(cross(dFdx(v_position), dFdy(v_position)));
    vec3 flatN = N;
    mat3 tbn = getTBN(N,v_texcoord0,v_position);
        N = mix(normalize(mul(tbn, getNormal(s_MatTexture, v_texcoord0))),N,v_extra.b);

    vec4 diffuse = texture2D(s_MatTexture,texuv);
#if !defined(ALPHA_TEST)
    if (rain > 0.001 && !env.underwater && !nether) {
        float numFrames = 60.0;
        float frameIndex = mod(floor(ViewPositionAndTime.w * 12.0), numFrames);
        float frameHeight = 1.0 / numFrames;
        vec2 rippleUV = vec2(pos_xz.x * 0.5, pos_xz.y * 0.5 * frameHeight + frameIndex * frameHeight);
        vec3 ripple = texture2D(s_Ripples, rippleUV).rgb;
        vec2 distortion = (ripple.xy - 0.5) * 0.08;
        vec2 distortedUV = texuv + (distortion * 0.125) * (1.0 - shadows);
         vec4 noRainDiffuse = diffuse;
        vec4 rainDiffuse = texture2D(s_MatTexture, distortedUV);
        
        diffuse = mix(diffuse, rainDiffuse, rain);
        diffuse = mix(diffuse,noRainDiffuse,nolight);
    }
#endif

  
  float endDist = FogAndDistanceControl.z*0.6;
    bool doEffect = (v_camdist < endDist);
  
  
    float alpha = diffuse.a;

bool isNonMetal = false;
bool isMetal = false;

#if !defined(SEASONS) && !(defined(ALPHA_TEST) || defined(OPAQUE))
isMetal = abs(alpha - 0.7) < 0.02;     // 0.68–0.72
isNonMetal = abs(alpha - 0.8) < 0.02;  // 0.78–0.82
#endif

vec4 color = v_color0;
    vec4 vertexcol = v_color1;
    float renderdistance = FogAndDistanceControl.z;
   
    vec3 waterPosition = cameraPos + v_wpos;
    
#ifdef ALPHA_TEST
    if (diffuse.a < 0.5) {
        discard;
    }
#endif

#if defined(SEASONS) && (defined(ALPHA_TEST) || defined(OPAQUE))
    diffuse.rgb *= mix(vec3_splat(1.0), 2.0 * texture2D(s_SeasonsTexture, v_color1.xy).rgb, vertexcol.y);
    diffuse.rgb *= vertexcol.aaa;
#else
    diffuse *= vertexcol;
#endif

    vec3 albedo = diffuse.rgb;
    
if(v_lava>0.9){
float bloomThreshold = 0.2;
float bloomStrength = 4.0;
vec2 texelSize = vec2(1.0/texuv.x, 1.0/texuv.y);
diffuse.rgb += bloomOptimized(s_MatTexture, texuv, texelSize, bloomThreshold) * bloomStrength;
}

      float jitter = fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453);
    vec3 rayOrigin = vec3(0.0, 6372.0e3, 0.0);
float dither = fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453);
   
    vec3 sunColor;
    vec3 scf;
    vec3 zenithsky = mainSkyRender(viewDir, sunDir, day, night, rain, dusk, dawn, snow, sunColor,time);
       vec3 ambientSky = mainSkyRender(normalize(vec3(0.0,1.0,0.0)), sunDir, day, night, rain, dusk, dawn, snow, scf,time);
     vec3 storeSky = zenithsky;
    
    
    if(nether){
    storeSky  = vec3(0.75, 0.25, 0.1);
    zenithsky = vec3(0.71, 0.23, 0.00)*1.2;
    sunColor  = vec3(1.0, 0.45, 0.1); 
    sunDir = normalize(vec3(0.3,1.0,0.3));
    ambientSky = vec3(0.71, 0.23, 0.00)*1.2;
    }else if(env.end){
    zenithsky = vec3(0.01,0.01,0.01);
    sunColor = vec3_splat(1.0);
    storeSky = vec3_splat(0.0);
    ambientSky = vec3_splat(0.2);
    }
    
          
    float ndotl = max(dot(N, sunDir), 0.0);

    sunColor = mix(sunColor, vec3_splat(1.0), rain);
    sunColor *=(1.0- shadows)* lm.y;
    vec3 blockLight = lightBlockCol(lm, time,v_color1.g,isCave);
  float finalAO = v_ao;
vec3 ambient = vec3_splat(0.03) + 
               (ambientSky*0.21)*finalAO;
vec3 shadowTint = mix(vec3(0.9, 0.95, 1.0), vec3(1.0, 1.0, 0.98), finalAO);
ambient *= shadowTint;
               
    vec3 diffuseLight =  dodiffuseLight(
    N,
    sunDir,
    viewDir,
    sunColor,
    ambient,
    0.0,
    sunVisibility,
    moonVisibility,
    lm.y,
    nolight
);     
    vec3 scattercol = vec3(0.5, 0.9, 0.4);

    // caves / nolight
    diffuse.rgb *= 1.0 - 0.8 * nolight;

     vec3 specular = brdf_specular(sunDir, viewPos, N, 0.24, vec3_splat(0.02), sunColor);
    float fastsss = hgPhase(dot(viewDir, sunDir), 0.76);
    
    vec3 lighting = diffuseLight + blockLight + specular;
    
    
    int dimensionID = int(GameDimension.x);
    bool isTheEnd = dimensionID == 2;

    if (!isTheEnd && !env.end) {
        diffuse.rgb *= lighting;
   
   diffuse.rgb *= 1.0 - 0.5 * night * lm_y;

#if defined(TRANSPARENT) && !(defined(SEASONS) || defined(RENDER_AS_BILLBOARDS))
    if (water) {
      diffuse.rgb = vec3_splat(1.0 - NL_WATER_TEX_OPACITY*(1.0 - diffuse.b*1.8));
}
#endif
if(nether){
if(v_lava>0.9){
   vec2 uv = waterPosition.xz;
   float scale = 0.1;
   float base = fractalTile(uv *scale);

    float clouds = valueNoiseTile(uv * 0.05, 8.0);
    clouds = smoothstep(0.1, 0.9, clouds);
    float combined = base * (0.4+ 1.0* clouds);
      
    combined = pow(combined, 1.5);
    float blur = (fractalTile(uv * scale) - fractalTile(uv * scale + 0.002));
    float rim = smoothstep(0.01, 0.08, blur);
    vec3 col = warmGradient(combined);
   diffuse.rgb*= col * 3.0 * (combined);
 
    }
 diffuse.rgb *=4.0;
}
}

vec3 basewaterCol = vec3(0.12, 0.28, 0.55);
bool blockUnderWater = (
        v_lightmapUV.y < 0.9 &&
        abs((2.0 * v_position.y - 15.0) / 16.0 - v_lightmapUV.y) < 0.00002
    );
  float depth = 1.0 - pow(lm.y,2.0);
   vec3 absorption;
   bool fromSurface = lm.y < 0.9 ;

    vec3 absorptionCoeff = vec3(2.5, 1.8, 1.2); // Red, Green, Blue
     absorption = exp(-depth * absorptionCoeff);
   
   if(water)
    diffuse.rgb *= absorption;
    
   vec3 deepScatter = vec3(0.1, 0.25, 0.5);   // Deep water color
    vec3 surfaceScatter = vec3(0.3, 0.6, 0.9); // Surface highlights
    
    float deepScatterAmount = depth * 1.5;
    deepScatterAmount = min(deepScatterAmount, 0.7);
    
    float surfaceScatterAmount = (1.0 - depth) * 0.4;
    
    vec3 totalScatter = deepScatter * deepScatterAmount + 
                       surfaceScatter * surfaceScatterAmount;
    
float phase = 0.7 + (depth * 0.5);
 totalScatter = totalScatter * phase * (1.0 - absorption);
    vec3 underWaterCol = mix(vec3(0.1, 0.25, 0.5),vec3(0.3, 0.6, 0.9),1.0-lm.y);


vec3 waterNormal; //out from water  
if(!isNonMetal){
 

diffuse.rgb = ApplyWaterEffect(
    diffuse.rgb,
    flatN,
    viewDir,
    viewPos,
    v_position,
    v_texcoord0,
    sunDir,
    FogColor.rgb,
    rayOrigin,
    day, night, rain, dusk, dawn, snow,
    time, jitter, dither,
    water,
    s_MatTexture,
    s_SunTex,
    s_SSRTex,
    s_NoiseVoxel,
    waterPosition,
    s_NoiseCausticsPuddles,
    s_MoonTex,
    v_texcoord0,
    diffuse.rgb,
    absorption,
    depth,
    waterNormal,
    s_WaveOne,
    s_WaveTwo,
    s_WaveThree,
    isCave
);
        }


float metallic = 0.0;
    if (isMetal){
       metallic= 0.0;
       }
       
vec3 F0 = mix(vec3_splat(0.04), albedo, metallic);
float f0 = 0.0;
float roughness;
if(isMetal){
roughness= 0.2;
f0 = 0.56;
}else{
roughness = 0.4;
f0 = 0.04;
}
float fresnel = fresnelSchlick(viewPos, N, f0);
 
#if !defined(WAVY_WATER)
if(water){
float fogDensity = depth * 0.5;
float visibility = exp(-fogDensity);
diffuse.a *= 0.3 + (visibility * 0.5);
}
#else
if(water){
float fogDensity = depth * 0.5;
float visibility = exp(-fogDensity);
diffuse.a *= 0.4 + (visibility * 0.5);
}
#endif

    vec3 fogColor = storeSky;   
   vec3 whichdirection = viewPos;
   
diffuse.rgb = ApplyPBRLighting(
    diffuse.rgba,
    albedo,
    viewDir,
    viewPos,
    N,
    sunDir,
    sunColor,
    storeSky,
    diffuseLight,
    roughness,
    F0,
    fresnel,
    time,
    dither,
    night,
    dawn,
    rain,
    jitter,
    isMetal,
    isNonMetal,
    doEffect,
    s_SunTex,
    s_NoiseVoxel
);


if (env.end) {
    diffuse.rgb = texture2D(s_MatTexture, v_texcoord0).rgb;
    //diffuse.rgb *= texture2D(s_LightMapTexture, lm).rgb;
    
    F0 = vec3_splat(0.04);
   // albedo *= 1.0 - F0;
 albedo = diffuse.rgb;
   
    vec3 L = normalize(vec3(-4.0, -1.0, -1.5));
    float diff = max(dot(N,L),0.0);
    
    vec3 diffuseLight = vec3_splat(1.0) * diff + vec3_splat(0.18);
    specular = brdf_specular(L, viewPos, N, 0.2, vec3_splat(0.02), vec3_splat(1.0));
  
    vec3 lightBlock = vec3(1.0,0.9,0.9) * lm.x * lm.x *lm.x *lm.x;
    
    diffuse.rgb *= diffuseLight + lightBlock + specular;
}

#if defined(ENABLE_CAUSTICS)
if(doEffect ){
diffuse.rgb = ApplyCaustics(
    diffuse.rgb,
    N,
    zenithsky,
    pos_xz,
    v_lightmapUV,
    v_position,
    time,
    env.underwater,
    nether,
    s_NoiseCausticsPuddles,
    viewDir,
    underWaterCol
);
}
#endif
  if (env.underwater) {
   vec3 watercol = mix(underWaterCol,vec3(0.02,0.1,0.3), night);
   vec2 gduv = -viewDir.xz/viewDir.y*18.2;
 float godrays = getGodRays(gduv,s_BlueNoise,time);
 
        fogColor = watercol;
        
        float brightn = 2.3;
              brightn += 0.3* dawn;
              brightn += 0.5*dusk;
              brightn+= 0.2 *night;
              
        diffuse.rgb *= brightn;
        #if !defined(REALISTIC_CAUSTICS)
        diffuse.rgb *= watercol;
        #endif
        
    }

if(!nether && !env.end){
    if(rain>0.5 && !isCave){
diffuse.rgb = ApplyPuddleEffect(
    diffuse.rgb,
    viewPos, viewDir, N,
    sunDir, sunColor, FogColor.rgb,
    waterPosition.xz, rain, snow, night, dawn,
    time, dither, water, doEffect, env.underwater,
    s_NoiseCausticsPuddles, s_SSRTex,s_NoiseVoxel,waterPosition
);}
   }
#if defined(SHOW_LIGHTNING)
if(!env.underwater && !isCave){
float flash = lightningFlash(time);
vec3 flashColor = vec3(1.0, 1.0, 0.9); // pale yellow-white
float upMask = smoothstep(0.2, 1.0, N.y);
float flicker = fract(sin(dot(vec2(time*60.0, flash*100.0), vec2(12.9898,78.233))) * 43758.5453);
flash *= (0.8 + 0.4 * flicker);
vec3 flashLight = flashColor * flash * 3.0; // intensity multiplier

diffuse.rgb *= 1.0 - 0.5 * rain;
diffuse.rgb += (flashLight * upMask)*rain;
}
#endif

#if defined(SHOW_SUNBEAMS)
if(cameraPos.y >= 60.0){
//float heightmask = smoothstep(60.0,80.0,cameraPos.y);
vec3 wpos = v_wpos;
float sunMask = smoothstep(0.2, 0.8, clamp(dot(normalize(wpos), sunDir), 0.0, 1.0));
vec3 beam = fbm(vec2(time*0.3+wpos.z/wpos.y))*sunColor;
vec3 sunbeam = mix(vec3_splat(0.0), beam, clamp(length(wpos)/16.0*1.7,0.0,1.0));
diffuse.rgb += ((sunbeam*0.65)*sunMask);
}
#endif

#if FOG_TYPE == 1
float fogalpha = getLayeredFog(v_wpos,cameraPos,vec2(80.0,60.0),FogAndDistanceControl.xy,v_godray);

#elif FOG_TYPE == 2//broken 
float fogalpha = CalcLayeredFogFactor(
        cameraPos,     // Camera position
        v_wpos,      // Fragment world position
        60.0,        // Fog layer height
        max(70.0+100.0*rain,0.0)         // Max fog distance
    );
#endif
    
    if(env.underwater){
        fogalpha= smoothstep(FogAndDistanceControl.x*0.6,FogAndDistanceControl.y,v_godray);
        }
        
   diffuse.rgb = mix(diffuse.rgb,fogColor,fogalpha*0.4);
if(nether){
    diffuse.rgb = colorCorrectionInv(diffuse.rgb);
    }else{
    diffuse.rgb =  colorCorrection(diffuse.rgb,rain);
       }
       
    gl_FragColor = diffuse;

//endif
