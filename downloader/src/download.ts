import {argv} from 'node:process';
import * as fs from 'node:fs/promises';
import * as path from 'node:path';
import * as puppeteer from 'puppeteer';

declare function jQuery(selector: string): any;

async function saveData(browser: puppeteer.Browser, url: string, outputFile: string) {
    const page = await browser.newPage();
    await page.goto(url);
    const pageData = await page.evaluate(() => {
        if ("data" in window) {
            return window.data as any;
        } else {
            throw new Error("No data found");
        }
    });
    await page.close();
    await fs.writeFile(outputFile, JSON.stringify(pageData, null, 2));
}

interface ResultsLink {
    date: string;
    link: string;
}

async function getList(
    browser: puppeteer.Browser, acblId: string, clubFilter: string
): Promise<ResultsLink[]> {
    const page = await browser.newPage();
    await page.goto(`https://my.acbl.org/club-results/my-results/${acblId}`);
    const output = await page.evaluate((clubFilter) => {
        jQuery('#DataTables_Table_0').DataTable().destroy();
        const rows = document.querySelectorAll('#DataTables_Table_0 tbody tr');
        const output = [];
        const clubRE = new RegExp(clubFilter, 'i');
        for (const row of rows) {
            const cells = row.getElementsByTagName('td');
            const clubName = cells[1].textContent || '';
            if (clubRE.test(clubName)) {
                const date = cells[0].textContent || '';
                const link = cells[8].getElementsByTagName('a')[0].href;
                output.push({date, link});
            }
        }
        return output;
    }, clubFilter);
    await page.close();
    return output;
}

async function main() {
    if (argv.length !== 5) {
        console.error(`Usage: node download.js <acblId> <clubName> <outputDir>`);
        process.exit(1);
    }
    const acblId = argv[2];
    const clubFilter = argv[3];
    const outputDir = argv[4];

    console.log('Starting browser');
    const browser = await puppeteer.launch();
    console.log('Getting list');
    const resultsList = await getList(browser, acblId, clubFilter);
    console.log(`Saving ${resultsList.length} results`);
    await fs.mkdir(outputDir, {recursive: true});
    const saves = resultsList.map(({date, link}) =>
        saveData(browser, link, path.join(outputDir, `${date}.json`)));
    await Promise.all(saves);
    await browser.close();
    console.log('Browser closed');
}

await main();
