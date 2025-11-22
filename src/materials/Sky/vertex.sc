#ifndef INSTANCING
$input a_color0, a_position
$output v_fogColor, v_worldPos, v_underwaterRainTimeDay, v_worldtestPos
#endif

#include <bgfx_shader.sh>
#ifndef INSTANCING
#include <newb/main.sh>

uniform vec4 FogColor;
uniform vec4 FogAndDistanceControl;
uniform vec4 ViewPositionAndTime;
uniform vec4 GameCameraPos;   // xyz = camera world position
#endif

void main() {
#ifndef INSTANCING
    v_underwaterRainTimeDay.x = float(detectUnderwater(FogColor.rgb, FogAndDistanceControl.xy));
    v_underwaterRainTimeDay.y = detectRain(FogAndDistanceControl.xyz);
    v_underwaterRainTimeDay.z = ViewPositionAndTime.w;
    v_underwaterRainTimeDay.w = detectDayFactor(FogColor.rgb);

    vec4 pos = vec4(a_position.xzy, 1.0);
    pos.xy = 2.0*clamp(pos.xy, -0.5, 0.5);
    
      vec3 worldPosition = mul(u_invViewProj, pos).xyz;
      vec3 worldDir = normalize(worldPosition.xyz);
    
    float rayDistance = 100000.0;
    v_worldPos = worldPosition ;
    v_worldtestPos = GameCameraPos.xyz + worldDir * rayDistance;
    v_fogColor = FogColor.rgb;

    gl_Position = pos;
#else
    gl_Position = vec4(0.0);
#endif
}