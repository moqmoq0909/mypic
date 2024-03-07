import { Stack, Chip, Tooltip } from "@mui/material";

import { CategoryTupple } from "@features/author/authorList/hooks/useGetTable";

type MultiTagsProps = {
  tupple: CategoryTupple[];
};

function MultiTags({ tupple }: MultiTagsProps) {
  const tagGroups = tupple.map((t) => {
    const tags = t[1].map((tag) => (
      <Tooltip key={`tag-${tag.tagCode}`} title={t[0].categoryName} placement={"top"}>
        <Chip
          key={`tag-${tag.tagCode}`}
          label={tag.tagName}
          size={"small"}
          sx={{ bgcolor: t[0].categoryColor, borderRadius: "5px" }}
        />
      </Tooltip>
    ));
    return (
      <Stack key={`tagGroup-${t[0].categoryCode}`} direction={"row"} spacing={1}>
        {tags}
      </Stack>
    );
  });
  return (
    <Stack spacing={1} alignItems={"center"} direction={"row"}>
      {tagGroups}
    </Stack>
  );
}

export default MultiTags;
