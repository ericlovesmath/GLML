import flatSDFViewer from "../../examples/2d_sdf_variants.glml?raw";
import mandelbrot from "../../examples/mandelbrot.glml?raw";
import mengerSponge from "../../examples/menger_sponge.glml?raw";
import raymarch from "../../examples/raymarch.glml?raw";
import warpedNoise from "../../examples/warped_noise.glml?raw";
import beaver from "../../examples/beaver.glml?raw";
import raymarchedMaterials from "../../examples/materials.glml?raw";
import raymarchedPlanet from "../../examples/planet.glml?raw";
import truchet from "../../examples/truchet.glml?raw";

import type { PassName } from "./renderer";

export const EXAMPLES: [string, string][] = [
  ["Mandelbrot", mandelbrot],
  ["Metallic Liquid", warpedNoise],
  ["Raymarched Planet", raymarchedPlanet],
  ["2D SDFs", flatSDFViewer],
  ["Beaver Mascot", beaver],
  ["Raymarching", raymarch],
  ["Menger Sponge", mengerSponge],
  ["Raymarch 3D Materials", raymarchedMaterials],
  ["Truchet Tiling", truchet],
];

export interface MultipassExample {
  name: string;
  passes: Partial<Record<PassName, string>>;
}

export const MULTIPASS_EXAMPLES: MultipassExample[] = []
