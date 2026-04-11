# Final Projects — Spring 2026

## Submission Instructions

Each group must create a subfolder in this directory named with your group number (e.g., `group-01`, `group-02`). Inside your group folder, include both your **tutorial** and **presentation slides**.

### Folder Structure

```
spring-2026/
├── group-01/
│   ├── tutorial/
│   │   ├── tutorial.pdf        (or .html or .md)
│   │   └── figures/            (if using Markdown)
│   │       ├── figure1.png
│   │       └── ...
│   └── presentation/
│       ├── slides.pdf          (or .html or .md)
│       └── figures/            (if using Markdown)
│           ├── figure1.png
│           └── ...
├── group-02/
│   └── ...
```

### Accepted File Formats

You may submit your tutorial and presentation slides as **PDF**, **Markdown (.md)**, or **HTML**.

### Making Sure Figures Display Correctly

The most common issue with submissions is broken figure links. Follow the guidelines for your chosen format:

#### PDF
- **Recommended for simplicity.** All figures are embedded in the file, so nothing extra is needed.
- If you generate your PDF from R Markdown or Quarto, knit/render to PDF locally and confirm the figures appear before uploading.

#### Markdown (.md)
- **You must include all figure image files** in a `figures/` subfolder next to your `.md` file.
- Use **relative paths** to reference images. For example:
  ```markdown
  ![Description of figure](figures/figure1.png)
  ```
- **Do not** use absolute paths (e.g., `C:/Users/...` or `/home/...`). These will break for everyone else.
- Make sure the image file names in your Markdown match the actual file names exactly (including capitalization).

#### HTML
- **Self-contained HTML is strongly preferred.** If you render from R Markdown or Quarto, use:
  - R Markdown: `self_contained: true` in your YAML header (this is the default).
  - Quarto: `embed-resources: true` in your YAML header.
- A self-contained HTML file embeds all figures as base64 data, so no separate image files are needed.
- If your HTML is **not** self-contained, you must include the accompanying `_files/` directory or a `figures/` folder with all referenced images, and ensure all paths are relative.

### Step-by-Step Upload Checklist

1. Create your group folder: `group-XX/`
2. Inside it, create `tutorial/` and `presentation/` subfolders.
3. Place your rendered files and any required figure files in the appropriate subfolder.
4. **Verify locally**: open your files from the repo folder and confirm all figures render. If you see broken images, fix the paths before pushing.
5. Commit and push your folder to this repository.

### Common Mistakes to Avoid

- Using absolute file paths for images instead of relative paths.
- Forgetting to include the `figures/` folder or `_files/` directory when submitting Markdown or non-self-contained HTML.
- File name mismatches (e.g., referencing `Figure1.png` when the file is `figure1.png`).
- Submitting an HTML file that references external resources without including them.
