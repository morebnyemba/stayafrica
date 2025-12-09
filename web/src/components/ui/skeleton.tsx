import { cn } from "@/lib/utils"

function Skeleton({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      className={cn("animate-pulse rounded-md bg-primary-200 dark:bg-primary-700", className)}
      {...props}
    />
  )
}

export { Skeleton }
