import { GridColDef, GridRenderCellParams, GridValueGetterParams } from "@mui/x-data-grid";

import { TableData } from "@features/author/authorList/hooks/useGetTable";
import { ATGridData, ATSubRows } from "@ui/dataGrid/AccordionTable";
import MultiTags from "@ui/tag/MultiTags";

type Record = {
  id: string;
  name: string;
};

const createDataGrid = ({ creators, books, images, tags }: TableData): ATGridData<Record> => {
  const tagsMap = new Map(tags);
  // カラム要素を定義
  const cols: GridColDef[] = [
    { field: "id", headerName: "コンテンツID", width: 120 },
    { field: "chil", headerName: "子要素", width: 90, editable: true },
    {
      field: "name",
      headerName: "名前",
      width: 400,
      editable: true,
    },
    {
      field: "tags",
      headerName: "タグ",
      width: 600,
      renderCell: (params: GridRenderCellParams) => {
        const tupple = tagsMap.get(params.row.id);
        return tupple ? <MultiTags tupple={tupple} /> : undefined;
      },
    },
    {
      field: "uploadDate",
      headerName: "日時",
      width: 200,
    },
    // {
    //   field: 'fullName',
    //   headerName: 'Full name',
    //   description: 'This column has a value getter and is not sortable.',
    //   sortable: false,
    //   width: 160,
    //   valueGetter: (params: GridValueGetterParams) =>
    //     `${params.row.firstName || ''} ${params.row.lastName || ''}`,
    // },
  ];
  return { rows: creators, subRows: books, cols };
};

export default createDataGrid;
