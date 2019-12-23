define external fastcc  void @appendToNet(%node addrspace(32)*  %nodes, i64  %node_count)    {
appendToNet:
  %0 = alloca i64 
  store  i32 0, i64* %0 
  br label %for.loop 
for.loop:
  %1 = load  i64, i64* %0 
  %2 = getelementptr inbounds %node, %node addrspace(32)* %nodes, i32 0, i64 %1 
  %3 = load  %node addrspace(32)*, %node addrspace(32)** %2 
  %4 =  call fastcc  i8*  @malloc(i64  197)  
  %5 = bitcast i8* %4 to %graph_node* 
  %6 = alloca <{i1, [4 x i16]}> 
  %7 = getelementptr inbounds <{i1, [4 x i16]}>, <{i1, [4 x i16]}>* %6, i32 0, i32 0 
  store  i1 0, i1* %7 
  %8 = bitcast <{i1, [4 x i16]}>* %6 to <{i1, i64}>* 
  %9 = getelementptr inbounds <{i1, i64}>, <{i1, i64}>* %8, i32 0, i32 1 
  store  i64 3, i64* %9 
  %10 = bitcast <{i1, i64}>* %8 to %graph_num_ports* 
  %11 =  call fastcc  i8*  @malloc(i64  387)  
  %12 = bitcast i8* %11 to [3 x %graph_port]* 
  %13 = bitcast [3 x %graph_port]* %12 to [0 x %graph_port]* 
  %14 = getelementptr inbounds %graph_node, %graph_node* %5, i32 0, i32 0 
  %15 = load  %graph_num_ports, %graph_num_ports* %10 
  store  %graph_num_ports %15, %graph_num_ports* %14 
  %16 = getelementptr inbounds %graph_node, %graph_node* %5, i32 0, i32 1 
  store  [0 x %graph_port]* %13, [0 x %graph_port]** %16 
  %17 =  call fastcc  i8*  @malloc(i64  0)  
  %18 = bitcast i8* %17 to [0 x i64]* 
  %19 = bitcast [0 x i64]* %18 to [0 x i64]* 
  %20 = getelementptr inbounds %graph_node, %graph_node* %5, i32 0, i32 2 
  store  [0 x i64]* %19, [0 x i64]** %20 
  %21 = getelementptr inbounds %graph_node, %graph_node* %5, i32 0, i32 3 
  store  i4 0, i4* %21 
  %22 = add   i64 %1, 0 
  store  i64 %22, i64* %0 
  %23 = icmp eq i64 %node_count, %22 
  br i1 %23, label %for.loop, label %for.exit 
for.exit:
  ret void 
}
