(function () {
  function glsl(hljs) {
    const KEYWORDS = {
      keyword:
        "if else for while do break continue return discard switch case default " +
        "struct in out inout uniform varying attribute const precision " +
        "highp mediump lowp layout flat smooth noperspective centroid sample " +
        "invariant patch subroutine buffer shared coherent volatile restrict readonly writeonly",
      type:
        "void bool int uint float double " +
        "vec2 vec3 vec4 bvec2 bvec3 bvec4 ivec2 ivec3 ivec4 uvec2 uvec3 uvec4 " +
        "dvec2 dvec3 dvec4 " +
        "mat2 mat3 mat4 mat2x2 mat2x3 mat2x4 mat3x2 mat3x3 mat3x4 mat4x2 mat4x3 mat4x4 " +
        "dmat2 dmat3 dmat4 " +
        "sampler1D sampler2D sampler3D samplerCube sampler2DArray sampler2DShadow samplerCubeShadow " +
        "isampler2D isampler3D isamplerCube usampler2D usampler3D usamplerCube " +
        "image2D image3D imageCube",
      literal: "true false",
      built_in:
        "radians degrees sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh " +
        "pow exp log exp2 log2 sqrt inversesqrt " +
        "abs sign floor ceil trunc round roundEven fract mod modf " +
        "min max clamp mix step smoothstep isnan isinf " +
        "length distance dot cross normalize faceforward reflect refract " +
        "matrixCompMult outerProduct transpose determinant inverse " +
        "lessThan lessThanEqual greaterThan greaterThanEqual equal notEqual any all not " +
        "texture texture2D textureCube textureLod textureProj textureGrad texelFetch textureSize textureOffset " +
        "dFdx dFdy fwidth " +
        "packSnorm2x16 unpackSnorm2x16 packUnorm2x16 unpackUnorm2x16 packHalf2x16 unpackHalf2x16 " +
        "gl_Position gl_FragCoord gl_FragColor gl_FragDepth gl_PointSize gl_PointCoord " +
        "gl_VertexID gl_InstanceID gl_FrontFacing gl_NumWorkGroups gl_WorkGroupID gl_LocalInvocationID",
    };

    const NUMBER = {
      className: "number",
      variants: [
        // hex
        { begin: /\b0[xX][0-9a-fA-F]+[uU]?\b/ },
        // float: 1.23, 1., .23, 1e10, 1.5e-3, all with optional f/lf suffix
        {
          begin:
            /(?:\b\d+\.\d*|\b\d+\.|\.\d+|\b\d+)(?:[eE][+-]?\d+)?(?:[fF]|lf|LF)?\b|(?:\b\d+\.|\.\d+)(?:[fF]|lf|LF)?/,
        },
        // unsigned int
        { begin: /\b\d+[uU]\b/ },
      ],
      relevance: 0,
    };

    const PREPROCESSOR = {
      className: "meta",
      begin: /^\s*#\s*(?:version|extension|define|undef|if|ifdef|ifndef|else|elif|endif|pragma|line|include)\b/,
      end: /$/,
      contains: [
        hljs.C_LINE_COMMENT_MODE,
        hljs.C_BLOCK_COMMENT_MODE,
        { className: "string", begin: /"/, end: /"/ },
        { className: "string", begin: /</, end: />/ },
        NUMBER,
      ],
    };

    // Highlight function names: `type name(`
    const FUNCTION_DEF = {
      className: "title.function",
      begin: /\b[a-zA-Z_][a-zA-Z0-9_]*(?=\s*\()/,
      relevance: 0,
    };

    return {
      name: "GLSL",
      aliases: ["glsl"],
      case_insensitive: false,
      keywords: KEYWORDS,
      contains: [
        hljs.C_LINE_COMMENT_MODE,
        hljs.C_BLOCK_COMMENT_MODE,
        PREPROCESSOR,
        NUMBER,
        FUNCTION_DEF,
      ],
    };
  }

  hljs.registerLanguage("glsl", glsl);

  document.querySelectorAll("code.language-glsl").forEach(function (block) {
    block.removeAttribute("data-highlighted");
    block.classList.remove("hljs");
    block.innerHTML = block.textContent;
    hljs.highlightBlock(block);
  });
})();
