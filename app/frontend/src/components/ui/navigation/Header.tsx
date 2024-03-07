import { Flex, Button, Box } from "@radix-ui/themes";

import MultiPullDownMenu from "@ui/navigation/multiPullDownMenu/MultiPullDownMenu";

function Header() {
  return (
    <Box
      position={"sticky"}
      style={{
        top: "0",
        boxShadow: "0 1px var(--gray-a4)",
        zIndex: 10, // MultiPullDownMenuのviewportが画像の下になるのを防ぐ
        // background: "rgb(25,25,25,0.8)",
        // backdropFilter: "blur(24px)", // すりガラスのような見た目になる
      }}
    >
      <MultiPullDownMenu />
    </Box>
  );
}
export default Header;
