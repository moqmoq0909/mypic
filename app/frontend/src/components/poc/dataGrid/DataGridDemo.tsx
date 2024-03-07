/** @jsxImportSource @emotion/react */
import React, { useState } from "react";
import { css } from "@emotion/react";
import { Box, Button } from "@mui/material";
import {
  DataGrid,
  GridColDef,
  GridValueGetterParams,
  GridRenderCellParams,
  GridRowClassNameParams,
  GridCellEditStopParams,
  GridCellParams,
  MuiEvent,
  MuiBaseEvent,
  GridRowParams,
  GridToolbarContainer,
  useGridApiContext,
} from "@mui/x-data-grid";
import { KeyboardArrowRight, KeyboardArrowDown } from "@mui/icons-material";

function CustomToolbar() {
  const apiRef = useGridApiContext();

  const handleGoToPage1 = () => {
    [...Array(9)].forEach((_, i) => {
      // apiRef.current.getCellElement(i + 1, 'firstName')
      apiRef.current.startCellEditMode({ id: i + 1, field: "firstName" });
      // apiRef.current.setEditCellValue({
      //   id: i + 1,
      //   field: 'firstName',
      //   value: 'テスト',
      // })
      apiRef.current.stopCellEditMode({ id: i + 1, field: "firstName" });
    });
    // console.log('apiRes:', apiRes)
  };

  return (
    <GridToolbarContainer>
      <Button onClick={handleGoToPage1}>API実行</Button>
    </GridToolbarContainer>
  );
}

type AccordionBtnType = {
  fn: (e: boolean) => void;
};

function AccordionBtn({ fn }: AccordionBtnType) {
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const handleOpen = (): void => {
    setIsOpen(true);
    fn(true);
  };
  const handleClose = (): void => {
    setIsOpen(false);
    fn(false);
  };

  return isOpen ? (
    <KeyboardArrowDown onClick={handleClose} />
  ) : (
    <KeyboardArrowRight onClick={handleOpen} />
  );
}

const chilMap = new Map();
chilMap.set(1, [
  {
    index: -1,
    id: 101,
    chil: true,
    lastName: "太郎",
    firstName: "田中",
    age: 35,
  },
  {
    index: -1,
    id: 102,
    chil: true,
    lastName: "二郎",
    firstName: "佐藤",
    age: 42,
  },
]);
chilMap.set(2, [
  {
    index: -1,
    id: 103,
    chil: true,
    lastName: "三郎",
    firstName: "田中",
    age: 35,
  },
  {
    index: -1,
    id: 104,
    chil: true,
    lastName: "四郎",
    firstName: "佐藤",
    age: 42,
  },
]);
chilMap.set(3, [
  {
    index: -1,
    id: 105,
    chil: true,
    lastName: "五郎",
    firstName: "田中",
    age: 35,
  },
  {
    index: -1,
    id: 106,
    chil: true,
    lastName: "六郎",
    firstName: "佐藤",
    age: 42,
  },
]);
chilMap.set(4, [
  {
    index: -1,
    id: 107,
    chil: true,
    lastName: "七郎",
    firstName: "田中",
    age: 35,
  },
  {
    index: -1,
    id: 108,
    chil: true,
    lastName: "八郎",
    firstName: "佐藤",
    age: 42,
  },
]);
chilMap.set(5, [
  {
    index: -1,
    id: 109,
    chil: true,
    lastName: "九郎",
    firstName: "田中",
    age: 35,
  },
  {
    index: -1,
    id: 110,
    chil: true,
    lastName: "十郎",
    firstName: "佐藤",
    age: 42,
  },
]);

const initRows = [
  { index: 1, id: 1, chil: false, lastName: "Snow", firstName: "Jon", age: 35 },
  {
    index: 2,
    id: 2,
    chil: false,
    lastName: "Lannister",
    firstName: "Cersei",
    age: 42,
  },
  {
    index: 3,
    id: 3,
    chil: false,
    lastName: "Lannister",
    firstName: "Jaime",
    age: 45,
  },
  {
    index: 4,
    id: 4,
    chil: false,
    lastName: "Stark",
    firstName: "Arya",
    age: 16,
  },
  {
    index: 5,
    id: 5,
    chil: false,
    lastName: "Targaryen",
    firstName: "Daenerys",
    age: null,
  },
  {
    index: 6,
    id: 6,
    chil: false,
    lastName: "Melisandre",
    firstName: null,
    age: 150,
  },
  {
    index: 7,
    id: 7,
    chil: false,
    lastName: "Clifford",
    firstName: "Ferrara",
    age: 44,
  },
  {
    index: 8,
    id: 8,
    chil: false,
    lastName: "Frances",
    firstName: "Rossini",
    age: 36,
  },
  {
    index: 9,
    id: 9,
    chil: false,
    lastName: "Roxie",
    firstName: "Harvey",
    age: 65,
  },
];

export default function DataGridDemo() {
  const [rows, setRows] = useState(initRows);

  const handleChilRows = (isOpen: boolean, params: GridRenderCellParams): void => {
    // const { rowNode, tabIndex } = params
    // console.log('rowNode:', rowNode)
    // console.log('tabIndex:', tabIndex)
    console.log("id:", params.row.id);
    console.log("isOpen:", isOpen);
    if (isOpen) {
      setRows((prevRows) => {
        const newRows = prevRows.concat();
        const chilRows = chilMap.get(params.row.id);
        console.log("chilRows:", chilRows);
        if (!chilRows) {
          return prevRows;
        }
        newRows.splice(params.row.index, 0, ...chilRows);
        const indexedRows = newRows.map((r, i) => Object.assign(r, { index: i + 1 }));
        console.log("prevRows:", prevRows);
        console.log("indexedRows:", indexedRows);
        return indexedRows;
      });
    } else {
      setRows((prevRows) => {
        const newRows = prevRows.concat();
        const chilRows = chilMap.get(params.row.id);
        console.log("chilRows:", chilRows);
        if (!chilRows) {
          return prevRows;
        }
        newRows.splice(params.row.index, chilRows.length);
        const indexedRows = newRows.map((r, i) => Object.assign(r, { index: i + 1 }));
        return indexedRows;
      });
    }
  };

  const columns: GridColDef[] = [
    {
      field: "btn",
      // headerName: '削除',
      sortable: false,
      width: 90,
      // disableClickEventBubbling: true,
      renderCell: (params: GridRenderCellParams) =>
        params.row.chil ? undefined : (
          <AccordionBtn fn={(isOpen: boolean) => handleChilRows(isOpen, params)} />
        ),
    },
    { field: "index", headerName: "Index", width: 90 },
    { field: "id", headerName: "ID", width: 90 },
    { field: "chil", headerName: "子要素", width: 90, editable: true },
    {
      field: "firstName",
      headerName: "First name",
      width: 150,
      editable: true,
    },
    {
      field: "lastName",
      headerName: "Last name",
      width: 150,
      editable: true,
    },
    {
      field: "age",
      headerName: "Age",
      type: "number",
      width: 110,
      editable: true,
    },
    {
      field: "fullName",
      headerName: "Full name",
      description: "This column has a value getter and is not sortable.",
      sortable: false,
      width: 160,
      valueGetter: (params: GridValueGetterParams) =>
        `${params.row.firstName || ""} ${params.row.lastName || ""}`,
    },
  ];

  // const handleCellEditStop = (
  //   params: GridCellParams,
  //   event: MuiEvent<MuiBaseEvent>
  // ) => {
  //   const v = event?.target?.value
  //   if (!v) return
  //   // console.log('params:', params)
  //   // console.log('event:', event)
  //   // console.log('value:', v)
  //   setRows((prevRows) =>
  //     prevRows.map((r) => {
  //       if (r.id === params.id) {
  //         // Object.assign(params.row, {params.field:v})
  //         const targetRow = { ...r }
  //         if (params.field in targetRow) {
  //           targetRow[params.field] = v
  //         }
  //         return targetRow
  //       }
  //       return r
  //     })
  //   )
  // }

  const boxCss = css`
    height: 800px;
    width: 100%;
    .childRow {
      background-color: #f2f2f2;
    }
    .MuiDataGrid-row.childRow:hover {
      background-color: #f2f2f2;
    }
    .MuiDataGrid-row.parentRow:hover {
      background-color: inherit;
    }
  `;

  return (
    <Box css={boxCss}>
      <DataGrid
        rows={rows}
        columns={columns}
        pageSize={10}
        rowsPerPageOptions={[5]}
        // checkboxSelection
        disableSelectionOnClick
        experimentalFeatures={{ newEditingApi: true }}
        getRowClassName={(param: GridRowClassNameParams) =>
          param.row.chil ? "childRow" : "parentRow"
        }
        // onCellEditStop={handleCellEditStop}
        // onRowEditStop={(param: GridRowParams) => {
        //   console.log('onRowEditStop:param:', param)
        // }}
        processRowUpdate={(newRow, oldRow) => {
          console.log("セル編集時のnewRow:", newRow);
          console.log("oldRow:", oldRow, "です！！");
          setRows((prevRows) => prevRows.map((r) => (r.id === newRow.id ? newRow : r)));
          // ここでサーバ側に値を送ってバリデーションすることも可能
          // 返り値はアップデートされ値を渡さなければならない
          // この返り値がMUI内のgetRowIdFromRowModel関数の引数となる
          return newRow;
        }}
        onProcessRowUpdateError={(error) => {
          // handle the error here
          console.error(error);
        }}
        components={{
          Toolbar: CustomToolbar,
        }}
      />
    </Box>
  );
}
