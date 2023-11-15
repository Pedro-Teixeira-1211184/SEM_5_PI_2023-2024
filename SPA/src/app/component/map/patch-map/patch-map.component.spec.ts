import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PatchMapComponent } from './patch-map.component';

describe('PatchMapComponent', () => {
  let component: PatchMapComponent;
  let fixture: ComponentFixture<PatchMapComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [PatchMapComponent]
    });
    fixture = TestBed.createComponent(PatchMapComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
