// Test script for verifying glossary data
const fs = require('fs');
const path = require('path');

// Import the glossary data
const glossaryDataPath = path.join(__dirname, '../src/data/glossary/index.js');
const glossaryMarkdownPath = path.join(__dirname, '../../HB conceptual glossary.md');

try {
    // Read the original glossary markdown
    const glossaryMd = fs.readFileSync(glossaryMarkdownPath, 'utf8');
    console.log('Successfully loaded original glossary markdown file');

    console.log('\n--- Original Glossary Analysis ---');
    const sectionCount = (glossaryMd.match(/^## /gm) || []).length;
    const categoryCount = (glossaryMd.match(/^### /gm) || []).length;
    const termCount = (glossaryMd.match(/^#### /gm) || []).length;
    console.log(`Markdown contains ${sectionCount} sections, ${categoryCount} categories, and ${termCount} terms`);

    // List first 5 terms for verification
    const termMatches = [...glossaryMd.matchAll(/^#### (.*?)$/gm)];
    console.log('\nFirst 5 terms from markdown:');
    termMatches.slice(0, 5).forEach((match, index) => {
        console.log(`${index + 1}. ${match[1]}`);
    });

    // Count terms with patterns/categories
    const architecturalPatterns = (glossaryMd.match(/^### Architectural Patterns\s+([^#]+)/s) || [''])[0];
    const patternCount = (architecturalPatterns.match(/^#### /gm) || []).length;
    console.log(`\nFound ${patternCount} architectural patterns`);

    const abbreviations = (glossaryMd.match(/^### Abbreviations\s+([^#]+)/s) || [''])[0];
    const abbrevCount = (abbreviations.match(/^- \*\*/gm) || []).length;
    console.log(`Found ${abbrevCount} abbreviations`);

    // Read and evaluate the processed glossary data
    // Note: We can't directly import ES modules in CommonJS
    // So we'll read the file and check its structure
    const glossaryJs = fs.readFileSync(glossaryDataPath, 'utf8');
    console.log('\n--- Processed Glossary Analysis ---');

    // Count exports by checking patterns
    const mainTerms = (glossaryJs.match(/glossaryTerms = \[\s+(.*?)\];/s) || [''])[0];
    const mainTermsCount = (mainTerms.match(/term: /g) || []).length;
    console.log(`Structured data contains ${mainTermsCount} main glossary terms`);

    const patterns = (glossaryJs.match(/architecturalPatterns = \[\s+(.*?)\];/s) || [''])[0];
    const patternsCount = (patterns.match(/term: /g) || []).length;
    console.log(`Structured data contains ${patternsCount} architectural patterns`);

    const abbrevs = (glossaryJs.match(/abbreviations = \[\s+(.*?)\];/s) || [''])[0];
    const abbrevsCount = (abbrevs.match(/term: /g) || []).length;
    console.log(`Structured data contains ${abbrevsCount} abbreviations`);

    const totalCount = mainTermsCount + patternsCount + abbrevsCount;
    console.log(`\nTotal terms in structured data: ${totalCount}`);

    // Check categories
    const categoryMatch = glossaryJs.match(/glossaryCategories = \[(.*?)\];/s);
    if (categoryMatch) {
        const categories = categoryMatch[1].split(',').map(c => c.trim().replace(/['"]/g, '')).filter(c => c);
        console.log(`\nCategories: ${categories.join(', ')}`);
        console.log(`Total categories: ${categories.length}`);
    }

    console.log('\n--- Data Validation Summary ---');
    if (mainTermsCount + patternsCount + abbrevsCount >= termCount) {
        console.log('✅ Glossary parsing SUCCESS: All terms from markdown appear to be processed');
    } else {
        console.log('❌ Glossary parsing WARNING: Some terms may be missing in the structured data');
        console.log(`   Original: ${termCount} terms, Processed: ${totalCount} terms`);
    }

} catch (error) {
    console.error('Error testing glossary data:', error);
}
