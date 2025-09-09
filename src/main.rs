use std::error::Error;
use std::fs::{read_to_string, write};
use rayon::prelude::*;

fn main() -> Result<(), Box<dyn Error>> {
    let s_values = vec![1.0, 1.15, 1.31, 1.375];
    let phantom_values = vec!["no-phantom", "with-phantom"];

    phantom_values.iter().for_each(|&phantom_value| {
        s_values.iter().for_each(|&s| {
            let read_path = format!("stats/k-vs-n-{}.csv", phantom_value);
            let raw_data = read_to_string(read_path).expect("Failed to read file");

            let rows: Vec<&str> = raw_data.trim().split('\n').collect();

            let k_avg = rows.par_iter().enumerate().map(|(n_pred, row)| {
                let values: Vec<f64> = row
                    .split(',')
                    .map(|x| x.trim().parse::<u32>().expect("Failed to parse number").into())
                    .collect();
                let sum: f64 = values.iter().sum();
                sum / ((n_pred + 1) as f64)
            }).collect::<Vec<f64>>();
            println!("Collected {} k-avg values for {}", phantom_value, s);

            let zipf: Vec<f64> = rows.par_iter().enumerate().map(|(i, row)| {
                // calculate h(n,s)
                let hns = (1..=i+1).map(|k| 1.0 / (k as f64).powf(s)).sum::<f64>();

                let values: Vec<f64> = row
                    .split(',')
                    .map(|x| x.trim().parse::<u32>().expect("Failed to parse number").into())
                    .collect();

                (1..=values.len())
                    .map(|k| values[k - 1] / (hns * (k as f64).powf(s)))
                    .sum()
            }).collect();
            println!("Collected {} zipf values for {}", phantom_value, s);

            let k_avg_output = k_avg
                .par_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n");

            write(
                format!("stats/k-avg-proof-{}-{}.csv", phantom_value, s),
                k_avg_output,
            ).expect("Failed to write file");

            let zipf_output = zipf
                .par_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n");

            write(
                format!("stats/n-vs-mean-proof-{}-{}.csv", phantom_value, s),
                zipf_output,
            ).expect("Failed to write file");
        });
    });

    Ok(())
}
