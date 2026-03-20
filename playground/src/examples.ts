import checkerboard from "../../examples/checkerboard.glml?raw";
import mandelbrot from "../../examples/mandelbrot.glml?raw";
import mouseCircle from "../../examples/mouse_circle.glml?raw";
import rainbow from "../../examples/rainbow.glml?raw";
import raymarch from "../../examples/raymarch.glml?raw";
import recursion from "../../examples/recursion.glml?raw";
import warpedNoise from "../../examples/warped_noise.glml?raw";
import flatSDFViewer from "../../examples/2d_sdf_variants.glml?raw";

export const EXAMPLES: [string, string][] = [
  ["Metallic Liquid", warpedNoise],
  ["Raymarching", raymarch],
  ["Mandelbrot", mandelbrot],
  ["2D SDFs + Variants", flatSDFViewer],
  ["Recursive Star", recursion],
  ["Mouse Tracking", mouseCircle],
  ["Pastel Rainbow", rainbow],
  ["Checkerboard", checkerboard],
];
