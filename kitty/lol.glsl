#version 330
uniform float time;
uniform vec2 resolution;
out vec4 frag_color;

float plasma(vec2 pos) {
    float c = 0.0;
    c += sin(pos.x * 2.0 + time);
    c += sin(pos.y * 3.0 - time);
    c += sin((pos.x + pos.y + time) * 4.0);
    c += sin(sqrt(pos.x*pos.x + pos.y*pos.y) * 5.0 - time * 2.0);
    return c / 4.0;
}

void main() {
    vec2 uv = gl_FragCoord.xy / resolution.xy;
    float color = plasma(uv * 10.0);
    frag_color = vec4(
        sin(color * 3.14159),
        cos(color * 2.71828),
        tan(color * 1.61803),
        1.0
    );
}
