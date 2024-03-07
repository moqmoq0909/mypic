import { useState, MouseEvent } from "react";
import { ToggleButtonGroup, ToggleButton, Menu, MenuItem } from "@mui/material";

type MenuItemOpt = {
  itemLabel: string;
  onClick: () => void;
};

type MenuOpt = {
  menuLabel: string;
  menuItems: MenuItemOpt[];
};

type MultiPullDownMenuProps = {
  menus: MenuOpt[];
};
function MultiPullDownMenu({ menus }: MultiPullDownMenuProps) {
  const [alignment, setAlignment] = useState<number | null>(null);
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const handleAlignment = (event: MouseEvent<HTMLElement>, newAlignment: number | null) => {
    setAlignment(newAlignment);
    setAnchorEl(event.currentTarget);
  };
  const handleMenuClose = () => {
    setAlignment(null);
    setAnchorEl(null);
  };
  const handleMenuItemClose = (fn: () => void) => () => {
    setAlignment(null);
    setAnchorEl(null);
    fn();
  };

  const toggleButtons = menus.map((m, i) => (
    <ToggleButton
      key={m.menuLabel}
      value={i}
      sx={{ color: "#ccc", "&.Mui-selected": { color: "#ccc" } }}
    >
      {m.menuLabel}
    </ToggleButton>
  ));
  const pullDownMenus = menus.map((m, i) => (
    <Menu
      key={m.menuLabel}
      anchorEl={anchorEl}
      open={!!(anchorEl && alignment === i)}
      onClose={handleMenuClose}
      // slot={{}}
      slotProps={{ paper: { square: true } }}
    >
      {m.menuItems.map((mit) => (
        <MenuItem
          key={`${m.menuLabel}_${mit.itemLabel}`}
          onClick={handleMenuItemClose(mit.onClick)}
        >
          {mit.itemLabel}
        </MenuItem>
      ))}
    </Menu>
  ));
  return (
    <>
      <ToggleButtonGroup
        value={alignment}
        exclusive
        onChange={handleAlignment}
        sx={{
          height: "28px",
          background: "#0f0f0f",
          border: "solid #0f0f0f",
          borderRadius: "0px",
        }}
      >
        {toggleButtons}
      </ToggleButtonGroup>
      {pullDownMenus}
    </>
  );
}

export default MultiPullDownMenu;
