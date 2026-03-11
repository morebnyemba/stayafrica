/**
 * AuthDivider - Shared divider with centered text for auth pages
 */

interface AuthDividerProps {
  text: string;
  /** Background class for the text span (must match surrounding card bg) */
  bgClassName?: string;
}

export function AuthDivider({
  text,
  bgClassName = 'bg-white',
}: AuthDividerProps) {
  return (
    <div className="relative my-6">
      <div className="absolute inset-0 flex items-center">
        <div className="w-full border-t border-primary-200" />
      </div>
      <div className="relative flex justify-center text-sm">
        <span className={`px-4 text-primary-500 ${bgClassName}`}>
          {text}
        </span>
      </div>
    </div>
  );
}
