#version 300 es

precision highp float;

uniform vec2 u_resolution;
uniform vec2 u_mouse;

out vec4 fragColor;

// SDF for an circle
float sdCircle(vec2 p, float r)
{
    return length(p) - r;
}

// SDF for an equilateral triangle
float sdTriangle(in vec2 p, in float r) {
    const float k = sqrt(3.0);
    p.x = abs(p.x) - r;
    p.y = p.y + r / k;
    if (p.x + k * p.y > 0.0) p = vec2(p.x - k * p.y, -k * p.x - p.y) / 2.0;
    p.x -= clamp(p.x, -2.0 * r, 0.0);
    return -length(p) * sign(p.y);
}

void main() {
    // Normalize coordinates (-1 to 1) and fix aspect ratio
    vec2 uv = (2.0 * gl_FragCoord.xy - u_resolution.xy) / min(u_resolution.y, u_resolution.x);
    vec2 mouseUV = (2.0 * u_mouse.xy - u_resolution.xy) / u_resolution.y;

    // Calculate distance to triangle + circle combo
    float d = max(sdTriangle(uv, 0.5), sdCircle(uv, 0.4));

    // Coloring logic (based on SDF distance)
    vec3 col = (d > 0.0) ? vec3(0.9, 0.6, 0.3) : vec3(0.65, 0.85, 1.0);
    col *= 1.0 - exp(-6.0 * abs(d));
    col *= 0.8 + 0.2 * cos(200.0 * d);
    col = mix(col, vec3(1.0), 1.0 - smoothstep(0.0, 0.01, abs(d)));

    // Add glow around mouse
    float mouseDist = length(uv - mouseUV);
    col += 0.2 * (1.0 / (mouseDist * 10.0));

    fragColor = vec4(col, 1.0);
}
