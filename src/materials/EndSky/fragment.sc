#ifndef INSTANCING
$input v_texcoord0, v_posTime
#endif

#include <bgfx_shader.sh>

#ifndef INSTANCING
#include <newb/config.h>

#include <resolute/includes.h>

SAMPLER2D_AUTOREG(s_CloudNoise);
uniform vec4 GameCameraPos;
SAMPLER2D_AUTOREG(s_SkyTexture);
#endif

void main() {
#ifndef INSTANCING
    vec3 color = vec3_splat(0.0);
    float time = v_posTime.w;
    
    float t = time;
      vec3 skyboxDir = v_posTime.xyz;
    
       vec4 bh = renderBlackhole(normalize(skyboxDir), time);
    color += bh.rgb;
    
    gl_FragColor = vec4(color, 1.0);
#else
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
#endif
}