# SESSION 2 QUICK START GUIDE

**Objective**: Complete property form, booking content, and global replacements  
**Estimated Time**: 5-6 hours  
**Priority**: COMPLETE

---

## 🎯 SESSION 2 CHECKLIST

### Phase 1: Property Form (90 mins)
- [ ] Open: `src/components/host/property-form.tsx`
- [ ] Find all `<input type="..."` elements (~15)
- [ ] Replace with `<Input type="..." />`
- [ ] Find `<textarea` elements (~2)
- [ ] Replace with `<Input multiline />`
- [ ] Find `<select` elements (~3)
- [ ] Replace with `<Input select options={...} />`
- [ ] Find `<button` elements (~3)
- [ ] Replace with `<Button` component
- [ ] Test on mobile device
- [ ] Commit: "chore: integrate Input/Button components into property form"

### Phase 2: Booking Content (20 mins)
- [ ] Open: `src/components/booking/booking-content.tsx`
- [ ] Find all `<button` elements (~5)
- [ ] Replace with `<Button` component
- [ ] Add Skeleton for loading state
- [ ] Test form submission
- [ ] Commit: "chore: integrate Button component into booking content"

### Phase 3: Global Button Replacement (60 mins)
- [ ] Search: `className=".*btn-primary"` (regex)
- [ ] Replace with: `<Button variant="primary">`
- [ ] Search: `className=".*btn-secondary"`
- [ ] Replace with: `<Button variant="secondary">`
- [ ] Search: `className=".*btn-outline"`
- [ ] Replace with: `<Button variant="outline">`
- [ ] Search: `className=".*btn-danger"`
- [ ] Replace with: `<Button variant="danger">`
- [ ] Test on multiple pages
- [ ] Commit: "chore: standardize all buttons to Button component"

### Phase 4: Global Input Replacement (60 mins)
- [ ] Search: `<input type="text"` (plain text)
- [ ] Replace with: `<Input type="text"`
- [ ] Search: `<input type="email"`
- [ ] Replace with: `<Input type="email"`
- [ ] Search: `<input type="password"`
- [ ] Replace with: `<Input type="password"`
- [ ] Search: `<select` (but not `<Input select`)
- [ ] Replace with: `<Input select options={...}`
- [ ] Search: `<textarea` (but not `<Input multiline`)
- [ ] Replace with: `<Input multiline`
- [ ] Test form validation
- [ ] Commit: "chore: standardize all inputs to Input component"

### Phase 5: Skeleton Updates (45 mins)
- [ ] Search: `import { Skeleton }` 
- [ ] Add: import `PropertyCardSkeleton`, `BookingPanelSkeleton`
- [ ] Replace generic skeletons with specific variants
- [ ] Update property list loading
- [ ] Update booking list loading
- [ ] Update profile loading
- [ ] Test animation performance
- [ ] Commit: "chore: update skeleton components with animations"

### Phase 6: Error Handling (30 mins)
- [ ] Update 404 page to use ErrorPages
- [ ] Update 500 page to use ErrorPages
- [ ] Wrap form pages with ErrorBoundary
- [ ] Ensure error messages display
- [ ] Commit: "chore: enhance error handling and pages"

### Phase 7: Testing & Verification (60 mins)
- [ ] Test all forms on mobile
- [ ] Verify validation feedback
- [ ] Check dark mode
- [ ] Check accessibility (Tab key, screen reader)
- [ ] Run manual smoke tests
- [ ] Document any issues
- [ ] Commit: "test: verify all component integrations"

---

## 📋 QUICK REFERENCE PATTERNS

### Input Replacement
```diff
- <input type="text" value={x} onChange={e => setX(e.target.value)} />
+ <Input type="text" label="Label" value={x} onChange={e => setX(e.target.value)} error={errors.x} />
```

### Button Replacement
```diff
- <button className="btn-primary">Submit</button>
+ <Button variant="primary">Submit</Button>
```

### Select Replacement
```diff
- <select onChange={e => setVal(e.target.value)}>
-   <option>...</option>
- </select>
+ <Input select value={val} onChange={e => setVal(e.target.value)} options={options} />
```

### Validation Pattern
```diff
- const result = validateEmail(email); if (!result.isValid) errors.email = result.error;
+ if (!validateEmail(email)) errors.email = 'Invalid email';
```

---

## 🔍 SEARCH COMMANDS

Use VS Code Find & Replace:

1. **Find btn-primary buttons**:
   ```
   Find: className="[^"]*btn-primary
   ```

2. **Find all input elements**:
   ```
   Find: <input\s+type="([^"]+)
   ```

3. **Find all select elements**:
   ```
   Find: <select
   Replace: <Input select
   ```

4. **Find all button elements**:
   ```
   Find: <button
   Replace: <Button
   ```

---

## ✅ COMPLETION CRITERIA

- [ ] All property form inputs updated
- [ ] All booking content buttons updated
- [ ] All btn-* classes replaced with Button component
- [ ] All input elements replaced with Input component
- [ ] All Skeleton usage updated
- [ ] Error pages enhanced
- [ ] Mobile responsive verified
- [ ] Form validation working
- [ ] Dark mode compatible
- [ ] Accessibility audit passed
- [ ] Zero console errors
- [ ] All tests passing

---

## 🚨 COMMON ISSUES & FIXES

### Issue: Input component missing value prop
**Fix**: Add `value` and `onChange`:
```typescript
<Input value={state} onChange={(e) => setState(e.target.value)} />
```

### Issue: Button click not firing
**Fix**: Ensure `type="button"` for non-submit buttons:
```typescript
<Button type="button" onClick={handleClick}>Click</Button>
```

### Issue: Validation error not showing
**Fix**: Add `error` prop to Input:
```typescript
<Input error={errors.email} />
```

### Issue: Form not submitting
**Fix**: Ensure button is inside form with `type="submit"`:
```typescript
<form onSubmit={handleSubmit}>
  <Input />
  <Button type="submit">Submit</Button>
</form>
```

### Issue: Skeleton animation not visible
**Fix**: Check Skeleton variant:
```typescript
import { PropertyCardSkeleton } from '@/components/ui';
// Use: <PropertyCardSkeleton count={3} />
```

---

## 📱 TESTING CHECKLIST

### Desktop Testing
- [ ] Chrome (latest)
- [ ] Firefox (latest)
- [ ] Edge (latest)
- [ ] Dark mode toggle

### Mobile Testing  
- [ ] iOS Safari (iPhone 12+)
- [ ] Android Chrome (latest)
- [ ] Touch interactions
- [ ] Form input focus
- [ ] Keyboard visibility

### Form Testing
- [ ] Email validation
- [ ] Password strength
- [ ] Required fields
- [ ] Error messages
- [ ] Success feedback
- [ ] Loading state

### Accessibility Testing
- [ ] Tab navigation
- [ ] Screen reader (NVDA/JAWS)
- [ ] Keyboard shortcuts
- [ ] Color contrast
- [ ] Focus indicators

---

## 📝 GIT COMMIT MESSAGES

```bash
# After each phase:
git add .
git commit -m "chore: [PHASE_NAME] - [description]"

# Examples:
git commit -m "chore: property-form - replace inputs with Input component"
git commit -m "chore: global-buttons - standardize button styling"
git commit -m "chore: global-inputs - standardize input styling"
git commit -m "chore: skeletons - update with animation variants"
git commit -m "chore: error-handling - enhance 404/500 pages"
git commit -m "test: verify component integrations and responsive design"
```

---

## 🎯 SUCCESS DEFINITION

**Session 2 is COMPLETE when**:
- ✅ All critical form pages use new Input component
- ✅ All buttons standardized to Button component
- ✅ All inputs standardized to Input component
- ✅ Skeleton loaders animated and working
- ✅ Error handling enhanced
- ✅ Zero console errors
- ✅ Mobile responsive verified
- ✅ Accessibility audit passed
- ✅ All tests green
- ✅ Code committed to main branch

---

## ⏱️ TIME BREAKDOWN

| Task | Est. Time | Actual |
|------|-----------|--------|
| Property Form | 90 mins | __ |
| Booking Content | 20 mins | __ |
| Global Buttons | 60 mins | __ |
| Global Inputs | 60 mins | __ |
| Skeletons | 45 mins | __ |
| Error Handling | 30 mins | __ |
| Testing | 60 mins | __ |
| **Total** | **~5.5 hours** | __ |

---

## 📞 SUPPORT

If stuck:
1. Check `INTEGRATION_GUIDE.md` for examples
2. Review `WEB_UI_UX_IMPROVEMENTS.md` for component docs
3. Look at completed files (login-content.tsx as reference)
4. Check component source files for prop definitions

---

**Session 2 Ready**: ✅  
**Target Completion**: ~5.5 hours (Jan 9, 2026)  
**Quality Target**: ⭐⭐⭐⭐⭐  

Let's go! 🚀
