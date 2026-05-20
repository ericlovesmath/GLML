(function () {
  function glsl(hljs) {
    return {
      name: "GLSL",
      aliases: ["glsl"],
      case_insensitive: false,
      keywords: {
        keyword:
          "if else for while do break continue return discard struct " +
          "in out inout uniform varying attribute const precision " +
          "highp mediump lowp layout flat smooth noperspective centroid sample " +
          "invariant true false",
        type:
          "void bool int uint float double " +
          "vec2 vec3 vec4 bvec2 bvec3 bvec4 ivec2 ivec3 ivec4 uvec2 uvec3 uvec4 " +
          "dvec2 dvec3 dvec4 " +
          "mat2 mat3 mat4 mat2x2 mat2x3 mat2x4 mat3x2 mat3x3 mat3x4 mat4x2 mat4x3 mat4x4 " +
          "sampler2D sampler3D samplerCube sampler2DArray sampler2DShadow samplerCubeShadow " +
          "isampler2D isampler3D isamplerCube usampler2D usampler3D usamplerCube",
        built_in:
          "radians degrees sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh " +
          "pow exp log exp2 log2 sqrt inversesqrt " +
          "abs sign floor ceil trunc round roundEven fract mod modf " +
          "min max clamp mix step smoothstep " +
          "length distance dot cross normalize faceforward reflect refract " +
          "matrixCompMult outerProduct transpose determinant inverse " +
          "lessThan lessThanEqual greaterThan greaterThanEqual equal notEqual any all not " +
          "texture textureLod textureProj textureGrad texelFetch textureSize " +
          "dFdx dFdy fwidth " +
          "gl_Position gl_FragCoord gl_FragColor gl_PointSize gl_PointCoord " +
          "gl_VertexID gl_InstanceID gl_FrontFacing",
      },
      contains: [
        hljs.C_LINE_COMMENT_MODE,
        hljs.C_BLOCK_COMMENT_MODE,
        {
          className: "meta",
          begin: /#\s*(version|extension|define|undef|if|ifdef|ifndef|else|elif|endif|pragma|line)/,
          end: /$/,
        },
        {
          className: "number",
          begin: /[+-]?\b\d+\.\d*([eE][+-]?\d+)?[fF]?\b/,
          relevance: 0,
        },
        {
          className: "number",
          begin: /[+-]?\b\d*\.\d+([eE][+-]?\d+)?[fF]?\b/,
          relevance: 0,
        },
        {
          className: "number",
          begin: /\b\d+[uU]?\b/,
          relevance: 0,
        },
      ],
    };
  }

  hljs.registerLanguage("glsl", glsl);

  document.querySelectorAll("code.language-glsl").forEach(function (block) {
    hljs.highlightBlock(block);
  });
})();
