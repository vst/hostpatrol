export default function MdxLayout({ children }: { children: React.ReactNode }) {
  return <div className="prose mx-auto my-6 flex  w-full max-w-4xl flex-col prose-pre:my-0 sm:my-20">{children}</div>;
}
