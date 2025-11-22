$input v_texcoord0, v_fogColor, v_worldPos, v_underwaterRainTime, v_worldDir

#include <bgfx_shader.sh>
#include <newb/main.sh>

SAMPLER2D_AUTOREG(s_MatTexture);

// obsolete now
// could be used for clouds, aurora?
 
  gl_FragColor = vec4_splat(0.0);
}
