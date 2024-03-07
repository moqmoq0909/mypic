/** @jsxImportSource @emotion/react */
import { useState } from "react";
import { css } from "@emotion/react";
import { Box, Button } from "@mui/material";
import {
  DataGrid,
  GridColDef,
  GridRenderCellParams,
  GridRowClassNameParams,
  GridToolbarContainer,
  useGridApiContext,
} from "@mui/x-data-grid";

import AccordionBtn from "@ui/button/AccordionBtn";

// 別コンポーネントに分ける
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

export type ATSubRows<T> = [string, T[]][];

export type ATGridData<T> = {
  rows: T[];
  subRows: ATSubRows<T>;
  cols: GridColDef[];
};

export type AccordionTableProps<T> = {
  gridData: ATGridData<T>;
};

// インデックス属性と親子属性の型
type AccordionTableRow<T> = {
  index: number;
  isOpen: boolean;
  chil: boolean;
} & T;
// インデックス属性と親子属性を付与したsubRowsの型
type MixedSubRows<T> = [string, AccordionTableRow<T>[]][];

export default function AccordionTable<Row extends { id: string }>({
  gridData,
}: AccordionTableProps<Row>) {
  const { rows: initRows, subRows: initSubRows, cols: initCols } = gridData;

  // rowsにインデックス属性と親子属性を付与
  const mixedrows = initRows.map((r, i) => ({
    index: i + 1,
    isOpen: false,
    chil: false,
    ...r,
  }));
  // 親と子の行を含めるためのstate,表示用(DBの更新には使用しない)
  const [visualRows, setVisualRows] = useState<AccordionTableRow<Row>[]>(mixedrows);
  // 親の行だけのstate(DBの更新時に使用)
  const [rows, setRows] = useState<AccordionTableRow<Row>[]>(mixedrows);

  // subRowsにインデックス属性と親子属性を付与
  // map内のprefixの「c」は「Category」の意
  // initSubRowsは元々カテゴリーごとでまとめられた配列になっている
  // MixedSubRowsというTuple型を使うことでkey,valueの順番を指定している
  const mixedSubRows: MixedSubRows<Row> = initSubRows.map(([cid, cSubrows], i) => [
    cid,
    cSubrows.map((csr) => ({ index: -1, isOpen: false, chil: true, ...csr })),
  ]);

  // 子の行だけのstate(DBの更新時に使用)
  const [subRows, setSubRows] = useState(new Map(mixedSubRows));

  // アコーディオンボタンが押下された時の処理
  const handleChilRows = (isOpen: boolean, params: GridRenderCellParams): void => {
    // console.log('id:', params.row.id)
    // console.log('isOpen:', isOpen)
    setVisualRows((prevRows) => {
      const newRows = [...prevRows];
      const chilRows = subRows.get(params.row.id);
      console.log("params.row.id:", params.row.id);
      console.log("chilRows:", chilRows);
      if (!chilRows) {
        return prevRows;
      }
      if (isOpen) newRows.splice(params.row.index, 0, ...chilRows);
      else newRows.splice(params.row.index, chilRows.length);
      const indexedRows = newRows.map((r, i) =>
        Object.assign(r, {
          index: i + 1,
          isOpen: r.id === params.row.id ? isOpen : r.isOpen,
        }),
      );
      // console.log('prevRows:', prevRows)
      // console.log('indexedRows:', indexedRows)
      return indexedRows;
    });
  };

  // アコーディオンボタンのカラムとインデックス用のカラム
  const defaultCols: GridColDef[] = [
    {
      field: "btn",
      // headerName: '削除',
      sortable: false,
      width: 90,
      // disableClickEventBubbling: true,
      renderCell: (params: GridRenderCellParams) =>
        params.row.chil ? undefined : (
          <AccordionBtn
            isOpen={params.row.isOpen}
            onClick={(isOpen: boolean) => handleChilRows(isOpen, params)}
          />
        ),
    },
    // { field: 'index', headerName: 'Index', width: 90 },
  ];
  const cols = [...defaultCols, ...initCols];

  return (
    <Box
      css={css`
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
      `}
    >
      <DataGrid
        rows={visualRows}
        columns={cols}
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
        // セル内のテキストを編集したときに呼ばれるイベント
        processRowUpdate={(newRow, oldRow) => {
          console.log("セル編集時のnewRow:", newRow);
          console.log("oldRow:", oldRow, "です！！");
          // newRowは編集された行
          // setRowsで編集された行のみ情報を更新する
          // (表示用)
          setVisualRows((prevRows) => prevRows.map((r) => (r.id === newRow.id ? newRow : r)));
          // (内部用 親の行)
          setRows((prevRows) => prevRows.map((r) => (r.id === newRow.id ? newRow : r)));
          // (内部用 子の行)
          setSubRows((prevSubRows) => {
            const subRowsArray = Array.from(prevSubRows.entries());
            // console.log('subRows配列へ変換', Array.from(prevSubRows.entries()))
            const newSubRowsArray: MixedSubRows<Row> = subRowsArray.map(([cid, cSubrows], i) => [
              cid,
              cSubrows.map((csr) => (csr.id === newRow.id ? newRow : csr)),
            ]);
            // console.log('newSubRowsArray', newSubRowsArray)
            return new Map(newSubRowsArray);
          });
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
