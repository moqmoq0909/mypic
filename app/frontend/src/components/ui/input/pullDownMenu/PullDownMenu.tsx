import { Button, DropdownMenu } from "@radix-ui/themes";
import "./styles.css";

export default function PullDownMenu() {
  return (
    <DropdownMenu.Root>
      <DropdownMenu.Trigger>
        <Button variant={"soft"} color={"orange"}>
          Menu
        </Button>
      </DropdownMenu.Trigger>
      <DropdownMenu.Content variant={"soft"} color={"orange"}>
        <DropdownMenu.Item>
          <a href={"/"}>Home</a>
        </DropdownMenu.Item>
        <DropdownMenu.Item>
          <a href={"/service"}>Service</a>
        </DropdownMenu.Item>
        <DropdownMenu.Separator />
        <DropdownMenu.Item>
          <a href={"/aboutus"}>About us</a>
        </DropdownMenu.Item>
        <DropdownMenu.Item>
          <a href={"/contact"}>Contact</a>
        </DropdownMenu.Item>
      </DropdownMenu.Content>
    </DropdownMenu.Root>
  );
}
