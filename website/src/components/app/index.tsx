'use client';

import { Just, Maybe, Nothing } from 'purify-ts/Maybe';
import { useEffect, useState } from 'react';
import { App } from './-app';
import { DataLoader, LhpData, deleteData, loadData } from './-data';
import { BigSpinner } from './-ui';

export function AppMain() {
  const [data, setAppData] = useState<Maybe<Maybe<LhpData[]>>>(Nothing);

  useEffect(() => {
    loadData().caseOf({
      Left(err) {
        console.error('Unexpected error while loading data.', err);
        console.error('Deleting data from local storage.');
        deleteData();
      },
      Right(md) {
        setAppData(Just(md));
      },
    });
  }, []);

  return data.caseOf({
    Nothing: () => <BigSpinner label="loading..." />,
    Just: (md) =>
      md.caseOf({
        Nothing: () => <DataLoader onLoadData={(x) => setAppData(Just(Just(x)))} />,
        Just: (d) => (
          <App
            data={d}
            onFlushRequest={() => {
              deleteData();
              setAppData(Just(Nothing));
            }}
          />
        ),
      }),
  });
}
