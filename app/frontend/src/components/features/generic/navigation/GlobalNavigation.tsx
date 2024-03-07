import { ReactNode } from "react";
import { Flex, Button, Box } from "@radix-ui/themes";

import Header from "@ui/navigation/Header";
import Footer from "@ui/navigation/Footer";

type GlobalNavigationProps = {
  children: ReactNode;
};

function GlobalNavigation({ children }: GlobalNavigationProps) {
  return (
    <Flex
      direction={"column"}
      style={{
        minWidth: "1200px",
        height: "100%",
        minHeight: "100vh",
        // background: "rgb(34,34,35)",
      }}
    >
      <Header />
      <Box>{children}</Box>
      <Box grow={"1"} height={"9"} />
      <Footer />
    </Flex>
  );
}
export default GlobalNavigation;
