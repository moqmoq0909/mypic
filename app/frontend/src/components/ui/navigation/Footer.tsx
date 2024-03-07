import { Flex, Box } from "@radix-ui/themes";

import TextField from "@ui/input/textField/TextField";

type FooterProps = {
  elem: JSX.Element;
};

function Footer() {
  return (
    <Box
      position={"sticky"}
      style={{
        width: "100%",
        height: "100px",
        // background: "rgb(54,53,56)",
        boxShadow: "0 -1px var(--gray-a4)",
        // フッターを下に固定するために必要
        bottom: "0",
      }}
    >
      <Flex justify={"center"}>
        <TextField />
      </Flex>
    </Box>
  );
}
export default Footer;
