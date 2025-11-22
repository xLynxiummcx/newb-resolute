vec3 bloomOptimized(sampler2D tex, vec2 uv, vec2 texelSize, float threshold) {
    vec3 sum = vec3(0.0, 0.0, 0.0);

    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            // Calculate Gaussian weights directly without array
            float weightX = 0.0;
            if (abs(x) == 0) weightX = 0.27901;
            else if (abs(x) == 1) weightX = 0.44198;
            else weightX = 0.27901;
            
            float weightY = 0.0;
            if (abs(y) == 0) weightY = 0.27901;
            else if (abs(y) == 1) weightY = 0.44198;
            else weightY = 0.27901;

            vec2 offset = vec2(float(x) * texelSize.x, float(y) * texelSize.y);
            vec3 sampleColor = texture2D(tex, uv + offset).rgb;

            // Extract only bright areas
            sampleColor = max(sampleColor - vec3(threshold, threshold, threshold), vec3(0.0, 0.0, 0.0));

            // Multiply by separable Gaussian weight
            sum += sampleColor * weightX * weightY;
        }
    }

    return sum;
}

vec3 AdjustSaturation(vec3 color, float saturation) {
    float luma = dot(color, vec3(0.2126, 0.7152, 0.0722));
    return mix(vec3_splat(luma), color, saturation);
}

vec3 aces_tonemap(vec3 color) {
    color *= 0.96;
    
    const float a = 2.51;
    const float b = 0.03;
    const float c = 2.43;
    const float d = 0.59;
    const float e = 0.14;
    return clamp((color * (a * color + b)) / (color * (c * color + d) + e), 0.0, 1.0);
}
vec3 filmic_contrast(vec3 color) {
    color = max(color, 0.0);
    color = 1.0 - exp(-color * 1.1);
    
    const vec3 lift = vec3(0.02, 0.01, 0.005);
    const vec3 gamma = vec3(1.3, 1.3, 1.3);
    const float gain = 1.26;
    
    color = (color + lift) * gain;
    color = pow(color, 1.0 / gamma);
    
    return color;
}

vec3 colorPost(vec3 col, float rain) {
    col = aces_tonemap(col);
    
    col = filmic_contrast(col);
    
    #if defined(SATURATION)
    col = AdjustSaturation(col,1.25);
    #endif
    
     return clamp(col, 0.0, 1.0);
}
vec3 colorCorrection(vec3 c,float r){
    return colorPost(c,r);
    }
    