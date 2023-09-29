import { Ok, Error } from "./gleam.mjs";
import fs from "node:fs";

export function readFile(path) {
  try {
    return new Ok(fs.readFileSync(path, "utf8"))
  } catch (error) {
    return new Error(error.toString())
  }
}