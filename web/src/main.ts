const GLSL_OUT = document.getElementById("glsl-output") as HTMLTextAreaElement;
const ERROR_OUT = document.getElementById("error-output") as HTMLDivElement;
const INPUT = document.getElementById("glml-input") as HTMLTextAreaElement;
const COMPILE = document.getElementById("compile-btn") as HTMLButtonElement;

const DEFAULT_EXAMPLE = `let main (coord : vec2) =
  [ coord.0, coord.1, 0.5 ]
`;

function compile(source: string): void {
  const result = window.glml.compile(source);
  if (result.glsl !== null) {
    GLSL_OUT.value = result.glsl;
    ERROR_OUT.textContent = "";
  } else {
    GLSL_OUT.value = "";
    ERROR_OUT.textContent = result.error ?? "Unknown error";
  }
}

function main(): void {
  INPUT.value = DEFAULT_EXAMPLE;

  COMPILE.addEventListener("click", () => {
    compile(INPUT.value);
  });

  compile(INPUT.value);
}

main();
