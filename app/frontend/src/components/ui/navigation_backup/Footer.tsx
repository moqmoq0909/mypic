import { AppBar } from "@mui/material";

type FooterProps = {
  elem: JSX.Element;
};

function Footer({ elem }: FooterProps) {
  return (
    <AppBar
      position={"sticky"}
      sx={{
        width: "100%",
        background: "rgb(28,63,84,0.8)",
        color: "rgb(255 255 255)",
        // フッターを下に固定するために必要
        bottom: "0",
      }}
      elevation={0}
    >
      {elem}
    </AppBar>
  );
}
export default Footer;
