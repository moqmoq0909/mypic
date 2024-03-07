const commonRules = require("./coding-rules/eslint/common");
const {
  frontendNamingConventionRules,
} = require("./coding-rules/eslint/naming-convention");
const jsxRules = require("./coding-rules/eslint/jsx");

module.exports = {
  // 上位ディレクトリへ設定ファイルを探索しない設定
  root: true,
  env: {
    browser: true,
    // モダンブラウザはes2023をサポートしている
    es2023: true,
  },
  extends: [
    // typescript-eslintの推奨設定 parser,pluginの設定が含まれる
    "plugin:@typescript-eslint/recommended",
    // Reactの推奨設定 plugins:["react"]も含まれている
    "plugin:react/recommended",
    // スタイルガイドはairbnbを指定
    "airbnb",
    "airbnb-typescript",
    "airbnb/hooks",
    // Prettierのルールで他の設定を上書きしたいので必ず最後に配置する
    "prettier",
  ],
  parserOptions: {
    // import/export文はES2015(es6)モジュールを指定
    sourceType: "module",
    // tsconfigの設定に準拠して、TypeScriptをパースする
    project: "./tsconfig.json",
  },
  rules: {
    // React v17以降はJSXのファイルに import React from 'react'; を記載しなくてよいので、ESLintで警告しない
    "react/react-in-jsx-scope": "off",
  },
  // ESLintが<tsconfigRootDir>/vite.config.tsというファイルを使用して構成されているが、対応するTSConfigにそのファイルが含まれていないため、エラーが発生している
  // vite.config.tsを対象から外せばいいのでESLintの ignorePatterns を用いて除外
  ignorePatterns: ["vite.config.ts"],
  // 拡張子ごとにルールを設定する
  overrides: [
    {
      // requireに対する構文上の警告を無視する
      files: ["*.js"],
      rules: { "@typescript-eslint/no-var-requires": "off" },
    },
    {
      // tsx,tsファイルに対する命名規則
      // 参考 https://typescript-jp.gitbook.io/deep-dive/styleguide
      files: ["*.ts", "*.tsx"],
      rules: {
        ...commonRules,
        ...frontendNamingConventionRules,
        ...jsxRules,
      },
    },
  ],
};

// Importパスにエイリアスを設定
// settings:
//   import/resolver:
//     typescript: []
