export default function MdxLayout({ children }: { children: React.ReactNode }) {
  return (
    <div className="prose mx-auto my-6 flex w-full  max-w-4xl flex-col px-4 prose-pre:my-0 sm:my-16">{children}</div>
  );
}
