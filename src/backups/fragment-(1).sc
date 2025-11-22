#ifndef INSTANCING
$input v_texcoord0, v_posTime
#endif

#include <bgfx_shader.sh>

#ifndef INSTANCING
  #include <newb/main.sh>

  SAMPLER2D_AUTOREG(s_SkyTexture);
#endif


void main() {
  #ifndef INSTANCING
  vec3 color;
  float time = v_posTime.w;
  vec3 viewDir = normalize(v_posTime.xyz);
  
  vec3 flashDir; 
  color = Endflash(viewDir,flashDir,time);
    vec4 bh = renderBlackhole(viewDir, time);
    color = bh.rgb;
     color = colorCorrection(color,0.0);

    gl_FragColor = vec4(color, 1.0);
  #else
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
  #endif
}
