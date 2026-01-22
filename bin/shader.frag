#version 300 es

precision highp float;

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

out vec4 fragColor;

// Exponential Smooth Minimum
float sMin(in float a, in float b, in float k) {
    k *= 1.0;
    float r = exp2(-a / k) + exp2(-b / k);
    return -k * log2(r);
}

// SDF for an circle
float sdCircle(in vec2 p, in float r) {
    return length(p) - r;
}

// SDF for an equilateral triangle
float sdTriangle(in vec2 p, in float r) {
    const float k = sqrt(3.0);

    p.x = abs(p.x) - r;
    p.y = p.y + r / k;

    if (p.x + k * p.y > 0.0) {
        p = vec2(p.x - k * p.y, -k * p.x - p.y) / 2.0;
    }
    p.x -= clamp(p.x, -2.0 * r, 0.0);
    return -length(p) * sign(p.y);
}

float sdBoxFrame(vec3 p, vec3 b, float e) {
    p = abs(p) - b;
    vec3 q = abs(p + e) - e;
    return min(min(
            length(max(vec3(p.x, q.y, q.z), 0.0)) + min(max(p.x, max(q.y, q.z)), 0.0),
            length(max(vec3(q.x, p.y, q.z), 0.0)) + min(max(q.x, max(p.y, q.z)), 0.0)),
        length(max(vec3(q.x, q.y, p.z), 0.0)) + min(max(q.x, max(q.y, p.z)), 0.0));
}

float sdSphere(in vec3 p) {
    return length(p) - 1.;
}

float sdTorus(vec3 p, vec2 t) {
    vec2 q = vec2(length(p.xz) - t.x, p.y);
    return length(q) - t.y;
}

mat2 rot2D(float angle) {
    float s = sin(angle);
    float c = cos(angle);
    return mat2(c, -s, s, c);
}

vec3 palette(float t) {
    return .5 + .5 * cos(6.28318 * (t + vec3(.3, .416, .557)));
}

void main() {
    // Normalize coordinates (-1 to 1) and fix aspect ratio
    vec2 uv = (2.0 * gl_FragCoord.xy - u_resolution.xy) / min(u_resolution.y, u_resolution.x);
    vec2 mouseUV = uv - (2.0 * u_mouse.xy - u_resolution.xy) / min(u_resolution.y, u_resolution.x);

    // Raymarching Logic
    vec3 ro = vec3(0, 0, -10); // ray origin
    vec3 rd = normalize(vec3(uv, 1)); // ray direction
    vec3 col = vec3(0);

    ro.yz *= rot2D(-mouseUV.y);
    rd.yz *= rot2D(-mouseUV.y);
    ro.xz *= rot2D(-mouseUV.x);
    rd.xz *= rot2D(-mouseUV.x);

    float t = 0.; // total distance travel

    for (int i = 0; i < 80; i++) {
        vec3 p = ro + rd * t;

        // Rotating Camera
        p.xy *= rot2D(u_time / 1000.);
        p.yz *= rot2D(u_time / 1000.);

        float d = sMin(sdTorus(p, vec2(1., 0.5)), sdBoxFrame(p, vec3(2, 2, 3), 0.5), 0.1);
        t += d;

        if (d < 0.001 || t > 100.) break;
    }

    col = palette(t * 0.3);
    if (t > 100.) col = vec3(0.2, 0.2, 0.2);

    // Add glow around mouse
    float mouseDist = length(mouseUV);
    col += 0.2 * (1.0 / (mouseDist * 10.0));

    fragColor = vec4(col, 1.0);

    // SDF visualization for 2D object below, from ShaderToy

    // Calculate distance to triangle + circle combo
    // float d = sMin(sdTriangle(uv, 0.5), sdCircle(uv - mouseUV, 0.3), 0.1);

    // Coloring logic (based on SDF distance)
    // vec3 col = (d > 0.0) ? vec3(0.9, 0.6, 0.3) : vec3(0.65, 0.85, 1.0);
    // col *= 1.0 - exp(-6.0 * abs(d));
    // col *= 0.8 + 0.2 * cos(200.0 * d);
    // col = mix(col, vec3(1.0), 1.0 - smoothstep(0.0, 0.01, abs(d)));

    // fragColor = vec4(col, 1.0);
}
