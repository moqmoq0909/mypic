import { TextField } from "@radix-ui/themes";
import { MagnifyingGlassIcon } from "@radix-ui/react-icons";

function TextFieldDemo() {
  return (
    <TextField.Root>
      <TextField.Slot>
        <MagnifyingGlassIcon height={"16"} width={"16"} />
      </TextField.Slot>
      <TextField.Input placeholder={"Search the docs…"} />
    </TextField.Root>
  );
}
export default TextFieldDemo;
