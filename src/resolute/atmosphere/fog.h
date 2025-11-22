
float getLayeredFog(vec3 worldPos, vec3 cameraPos, vec2 fogHeights, vec2 fogDensity, float dist) {
    // fogHeights.x = bottom layer height
    // fogHeights.y = top layer height
    // fogDensity.x = near layer density
    // fogDensity.y = upper layer density

    float heightFactor = clamp((worldPos.y - fogHeights.x) / (fogHeights.y - fogHeights.x), 0.0, 1.0);
    float camHeightFactor = clamp((cameraPos.y - fogHeights.x) / (fogHeights.y - fogHeights.x), 0.0, 1.0);
    float density = mix(fogDensity.x, fogDensity.y, heightFactor);
    float camDensity = mix(fogDensity.x, fogDensity.y, camHeightFactor);
    float layerVisibility = 1.0 - smoothstep(0.0, 1.0, camHeightFactor);
    float fogFactor = 1.0 - exp(-dist * (density + camDensity) * 0.5);
    fogFactor *= layerVisibility;
    return clamp(fogFactor, 0.0, 1.0);
}

float CalcLayeredFogFactor(
    vec3 cameraWorldPos,    // Camera position in world space
    vec3 pixelWorldPos,     // Fragment/pixel position in world space
    float fogTop,           // Height of fog layer top 
    float fogEnd            // Maximum fog di
)
{
    vec3 CameraProj = cameraWorldPos;
    CameraProj.y = 0.0;
    
    vec3 PixelProj = pixelWorldPos;
    PixelProj.y = 0.0;
    
    float DeltaD = length(CameraProj - PixelProj) / fogEnd;
    

    float DeltaY = 0.0;
    float DensityIntegral = 0.0;
    if (cameraWorldPos.y > fogTop) {

        if (pixelWorldPos.y < fogTop) {

            DeltaY = (fogTop - pixelWorldPos.y) / fogTop;
            DensityIntegral = DeltaY * DeltaY * 0.5;
        }

    } 
    else {
        if (pixelWorldPos.y < fogTop) {

            DeltaY = abs(cameraWorldPos.y - pixelWorldPos.y) / fogTop;
            float DeltaCamera = (fogTop - cameraWorldPos.y) / fogTop;
            float DensityIntegralCamera = DeltaCamera * DeltaCamera * 0.5;
            float DeltaPixel = (fogTop - pixelWorldPos.y) / fogTop;
            float DensityIntegralPixel = DeltaPixel * DeltaPixel * 0.5;
            DensityIntegral = abs(DensityIntegralCamera - DensityIntegralPixel);
        } 
        else {
            DeltaY = (fogTop - cameraWorldPos.y) / fogTop;
            DensityIntegral = DeltaY * DeltaY * 0.5;
        }
    }
    
    float FogDensity = 0.0;
    
    if (DeltaY != 0.0) {
        float PathLengthFactor = sqrt(1.0 + ((DeltaD / DeltaY) * (DeltaD / DeltaY)));
        FogDensity = PathLengthFactor * DensityIntegral;
    }
    
    return FogDensity;
}
