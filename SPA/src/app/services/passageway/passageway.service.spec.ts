import { TestBed } from '@angular/core/testing';

import { PassagewayService } from './passageway.service';

describe('PassagewayService', () => {
  let service: PassagewayService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(PassagewayService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
