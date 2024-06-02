error id: KTn+kvHrUFFPSVBsasZynQ==
### Bloop error:

Unexpected error when copying <WORKSPACE>/.bloop/lab05_transpiler/bloop-internal-classes/classes-Metals-LkPaNwoxRGCIP-LUJcViAQ==-VR3qZX3BS5atLH0GOYfz8g==/alpine/codegen/ScalaPrinter$package.tasty to <WORKSPACE>/.bloop/lab05_transpiler/bloop-bsp-clients-classes/classes-Metals-LkPaNwoxRGCIP-LUJcViAQ==/alpine/codegen/ScalaPrinter$package.tasty, you might need to restart the build server.
java.nio.file.FileAlreadyExistsException: <WORKSPACE>/.bloop/lab05_transpiler/bloop-bsp-clients-classes/classes-Metals-LkPaNwoxRGCIP-LUJcViAQ==/alpine/codegen/ScalaPrinter$package.tasty
	at java.base/sun.nio.fs.UnixException.translateToIOException(UnixException.java:94)
	at java.base/sun.nio.fs.UnixException.rethrowAsIOException(UnixException.java:106)
	at java.base/sun.nio.fs.UnixException.rethrowAsIOException(UnixException.java:111)
	at java.base/sun.nio.fs.UnixCopyFile.copyFile(UnixCopyFile.java:246)
	at java.base/sun.nio.fs.UnixCopyFile.copy(UnixCopyFile.java:603)
	at java.base/sun.nio.fs.UnixFileSystemProvider.copy(UnixFileSystemProvider.java:258)
	at java.base/java.nio.file.Files.copy(Files.java:1305)
	at bloop.io.ParallelOps$.copy$1(ParallelOps.scala:164)
	at bloop.io.ParallelOps$.$anonfun$copyDirectories$8(ParallelOps.scala:189)
	at scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.java:23)
	at monix.eval.internal.TaskRunLoop$.startFuture(TaskRunLoop.scala:494)
	at monix.eval.Task.runToFutureOpt(Task.scala:586)
	at monix.eval.internal.TaskDeprecated$Extensions.runSyncMaybeOptPrv(TaskDeprecated.scala:128)
	at monix.eval.internal.TaskDeprecated$Extensions.$anonfun$coeval$1(TaskDeprecated.scala:303)
	at monix.eval.Coeval$Always.apply(Coeval.scala:1451)
	at monix.eval.Coeval.value(Coeval.scala:258)
	at bloop.io.ParallelOps$.$anonfun$copyDirectories$7(ParallelOps.scala:229)
	at monix.reactive.internal.consumers.ForeachAsyncConsumer$$anon$1.onNext(ForeachAsyncConsumer.scala:44)
	at monix.reactive.internal.consumers.LoadBalanceConsumer$$anon$1.$anonfun$signalNext$1(LoadBalanceConsumer.scala:218)
	at monix.execution.internal.InterceptRunnable.run(InterceptRunnable.scala:27)
	at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)
	at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)
	at java.base/java.lang.Thread.run(Thread.java:833)
#### Short summary: 

Unexpected error when copying <WORKSPACE>/.bloop/lab05_transpiler/bloop-internal-classes/classes-Met...