import { AppBar } from "@mui/material";

type HeaderProps = {
  elem: JSX.Element;
};

function Header({ elem }: HeaderProps) {
  return (
    <AppBar
      position={"sticky"}
      sx={{ width: "100%", background: "rgb(72 98 113)", color: "rgb(255 255 255)" }}
      elevation={0}
    >
      {elem}
    </AppBar>
  );
}
export default Header;
