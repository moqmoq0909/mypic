import { Box } from "@mui/material";

import Header from "@ui/navigation_backup/Header";
import Footer from "@ui/navigation_backup/Footer";
import { ReactNode } from "react";

type GlobalNavigationProps = {
  children: ReactNode;
};

function GlobalNavigation({ children }: GlobalNavigationProps) {
  return (
    <Box
      sx={{
        minWidth: "700px",
        height: "100%",
        minHeight: "100vh",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <Header elem={<>ヘッダー</>} />
      <Box sx={{ m: 2 }}>{children}</Box>
      <Box flexGrow={1} />
      <Footer elem={<>フッター</>} />
    </Box>
  );
}
export default GlobalNavigation;
