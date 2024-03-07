import * as React from "react";
import {
  DataGrid,
  GridColDef,
  GridRowModel,
  GridRenderEditCellParams,
  useGridApiContext,
  GridColTypeDef,
} from "@mui/x-data-grid";
import InputBase, { InputBaseProps } from "@mui/material/InputBase";
import Popper from "@mui/material/Popper";
import Paper from "@mui/material/Paper";
import { randomInt, randomUserName, randomArrayItem } from "@mui/x-data-grid-generator";

import withNavigation from "../navigation/withNavigation";

const lines = [
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
  "Aliquam dapibus, lorem vel mattis aliquet, purus lorem tincidunt mauris, in blandit quam risus sed ipsum.",
  "Maecenas non felis venenatis, porta velit quis, consectetur elit.",
  "Vestibulum commodo et odio a laoreet.",
  "Nullam cursus tincidunt auctor.",
  "Sed feugiat venenatis nulla, sit amet dictum nulla convallis sit amet.",
  "Nulla venenatis justo non felis vulputate, eu mollis metus ornare.",
  "Nam ullamcorper ligula id consectetur auctor.",
  "Phasellus et ultrices dui.",
  "Fusce facilisis egestas massa, et eleifend magna imperdiet et.",
  "Pellentesque ac metus velit.",
  "Vestibulum in massa nibh.",
  "Vestibulum pulvinar aliquam turpis, ac faucibus risus varius a.",
];

function EditTextarea(props: GridRenderEditCellParams<string>) {
  const { id, field, value, colDef } = props;
  const [valueState, setValueState] = React.useState(value);
  const [anchorEl, setAnchorEl] = React.useState<HTMLElement | null>();
  const apiRef = useGridApiContext();

  const handleRef = React.useCallback((el: HTMLElement | null) => {
    setAnchorEl(el);
  }, []);

  const handleChange = React.useCallback<NonNullable<InputBaseProps["onChange"]>>(
    (event) => {
      const newValue = event.target.value;
      setValueState(newValue);
      apiRef.current.setEditCellValue({ id, field, value: newValue, debounceMs: 200 }, event);
    },
    [apiRef, field, id],
  );

  const handleKeyDown = React.useCallback<NonNullable<InputBaseProps["onKeyDown"]>>(
    (event) => {
      if (
        event.key === "Escape" ||
        (event.key === "Enter" && !event.shiftKey && (event.ctrlKey || event.metaKey))
      ) {
        const params = apiRef.current.getCellParams(id, field);
        apiRef.current.publishEvent("cellKeyDown", params, event);
      }
    },
    [apiRef, id, field],
  );

  return (
    <div style={{ position: "relative", alignSelf: "flex-start" }}>
      <div
        ref={handleRef}
        style={{
          height: 1,
          width: colDef.computedWidth,
          display: "block",
          position: "absolute",
          top: 0,
        }}
      />
      {anchorEl && (
        <Popper open anchorEl={anchorEl} placement={"bottom-start"}>
          <Paper elevation={1} sx={{ p: 1, minWidth: colDef.computedWidth }}>
            <InputBase
              multiline
              rows={4}
              value={valueState}
              sx={{ textarea: { resize: "both" }, width: "100%" }}
              onChange={handleChange}
              autoFocus
              onKeyDown={handleKeyDown}
            />
          </Paper>
        </Popper>
      )}
    </div>
  );
}

const multilineColumn: GridColTypeDef = {
  type: "string",
  renderEditCell: (params) => <EditTextarea {...params} />,
};

const columns: GridColDef[] = [
  { field: "id", headerName: "ID" },
  { field: "username", headerName: "Name", width: 150 },
  { field: "age", headerName: "Age", width: 80, type: "number" },
  {
    field: "bio",
    headerName: "Bio",
    width: 400,
    editable: true,
    ...multilineColumn,
  },
];

const rows: GridRowModel[] = [];

for (let i = 0; i < 50; i += 1) {
  const bio = [];

  for (let j = 0; j < randomInt(1, 7); j += 1) {
    bio.push(randomArrayItem(lines));
  }

  rows.push({
    id: i,
    username: randomUserName(),
    age: randomInt(10, 80),
    bio: bio.join(" "),
  });
}

function MultilineEditDemo() {
  return (
    <div style={{ height: 300, width: "100%" }}>
      <DataGrid rows={rows} columns={columns} experimentalFeatures={{ newEditingApi: true }} />
    </div>
  );
}

// export default MultilineEditDemo
export default withNavigation(MultilineEditDemo, "検索画面");
