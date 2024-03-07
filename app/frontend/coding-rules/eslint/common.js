module.exports = {
  //varの使用を禁止
  "no-unused-vars": "off",
  "@typescript-eslint/no-unused-vars": "error",
  // thisの後のアンダースコア(_)を許可する
  "no-underscore-dangle": ["error", { allowAfterThis: true }],
  // namespaceは使わずESModulesのexportを使用する
  // 参考 https://typescript-jp.gitbook.io/deep-dive/project/namespaces
  "@typescript-eslint/no-namespace": "error",
  // enumの使用を禁止
  // 参考 https://typescriptbook.jp/reference/values-types-variables/enum/enum-problems-and-alternatives-to-enums
  "no-restricted-syntax": [
    "error",
    {
      selector: "TSEnumDeclaration",
      message: "Don't declare enums",
    },
  ],
  // null,undefinedは極力使わない
  // オブジェクトの値の存在チェックはオプショナルチェーンを使う
  "@typescript-eslint/prefer-optional-chain": "error",
  // Null 合体演算子(??)の使用を推奨する
  "@typescript-eslint/prefer-nullish-coalescing": "error",
  // 文字列はダブルクオートで囲む
  // 入れ子になる場合は代わりにバッククオートを使う
  quotes: ["error", "double", { allowTemplateLiterals: true }],
  // セミコロンは常に書く
  // 参考 https://qiita.com/mysticatea/items/9da94240f29ea516ae87
  semi: ["error", "always"],
  "semi-spacing": ["error", { after: true, before: false }],
  "semi-style": ["error", "last"],
  "no-extra-semi": "error",
  "no-unexpected-multiline": "error",
  "no-unreachable": "error",
  // 配列の宣言は配列コンストラクタ(new Array())ではなく配列リテラル([])を使う
  "@typescript-eslint/array-type": ["error", { default: "array", readonly: "array" }],
  // 型定義やクラスのインターフェースの実装はinterfaceではなくtypeを使用する
  "@typescript-eslint/consistent-type-definitions": ["error", "type"],
};
