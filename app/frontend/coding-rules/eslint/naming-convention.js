// フロントエンド・バックエンドともに共通の命名規則ルール
const commonNamingConventionRules = [
  // const,letはキャメルケースを使用
  {
    selector: "variable",
    format: ["strictCamelCase"],
  },
  // boolean型の変数はprefixにisもしくはhasを矯正
  {
    selector: "variable",
    types: ["boolean"],
    format: ["StrictPascalCase"],
    prefix: ["is", "has"],
  },
  // 関数の引数はキャメルケースを使用
  {
    selector: "parameter",
    format: ["strictCamelCase"],
    leadingUnderscore: "allow",
  },
  // クラスはパスカルケースを使用
  {
    selector: "class",
    format: ["StrictPascalCase"],
  },
  // クラスのprivateプロパティはprefixにアンダースコア(_)を矯正
  // ※modifiersを含むルールは通常のルールより上に書く
  {
    selector: "classProperty",
    modifiers: ["private"],
    format: ["strictCamelCase"],
    prefix: ["_"],
  },
  // クラスのプロパティはキャメルケースを使用
  {
    selector: "classProperty",
    format: ["strictCamelCase"],
  },
  // クラスのメソッドはキャメルケースを使用
  {
    selector: "classMethod",
    format: ["strictCamelCase"],
  },
  // get,setはキャメルケースを使用
  {
    selector: "accessor",
    format: ["strictCamelCase"],
  },
  // typeはパスカルケースを使用
  {
    selector: "typeAlias",
    format: ["StrictPascalCase"],
  },
];

// フロントエンド側の設定
const frontendNamingConventionRules = {
  "@typescript-eslint/naming-convention": [
    "error",
    // 関数コンポーネントはfunction宣言を使いパスカルケースで命名する
    // 単なる関数やhooksはアロー関数式を使いキャメルケースで命名する(変数扱いとなる)
    {
      selector: "function",
      format: ["StrictPascalCase"],
    },
    ...commonNamingConventionRules,
  ],
};

// バックエンド側の設定
const backendNamingConventionRules = {
  "@typescript-eslint/naming-convention": [
    "error",
    // 関数もキャメルケースで命名する
    {
      selector: "function",
      format: ["strictCamelCase"],
    },
    ...commonNamingConventionRules,
  ],
};

module.exports = {
  frontendNamingConventionRules,
  backendNamingConventionRules,
};
