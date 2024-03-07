import { Text, Flex, Switch } from "@radix-ui/themes";

function SwitchDemo({ onClick, text }) {
  return (
    <Text as={"label"} size={"2"}>
      <Flex gap={"2"}>
        <Switch onClick={onClick} size={"3"} /> {text}
      </Flex>
    </Text>
  );
}

export default SwitchDemo;
