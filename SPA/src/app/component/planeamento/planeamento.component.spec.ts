import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PlaneamentoComponent } from './planeamento.component';

describe('PlaneamentoComponent', () => {
  let component: PlaneamentoComponent;
  let fixture: ComponentFixture<PlaneamentoComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [PlaneamentoComponent]
    });
    fixture = TestBed.createComponent(PlaneamentoComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
