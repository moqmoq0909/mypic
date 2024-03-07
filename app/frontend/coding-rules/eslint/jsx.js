module.exports = {
  //コンポーネントにプロパティを渡す時はダブルクオートではなく波括弧を使う
  "react/jsx-curly-brace-presence": [
    "error",
    { props: "always", children: "never", propElementValues: "always" },
  ],
  /*
    オプション引数にデフォルト値が設定されていないことでエラーが出るが、
    ESLintの対応が遅く、非推奨とされる修正方法を提案してくるためルールを無効にする
  */
  "react/require-default-props": "off",
};
