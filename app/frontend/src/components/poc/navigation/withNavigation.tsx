import { ReactNode, ComponentType, createElement } from "react";

export default function withNavigation<Props extends {} = {}>(
  component: ComponentType<Props>,
  title: ReactNode,
) {
  return function View(props: Props) {
    return (
      <>
        <div>{title}</div>
        {createElement(component, props)}
      </>
    );
  };
}
